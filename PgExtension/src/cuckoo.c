#include "postgres.h"

#include "fmgr.h"
#include "utils/builtins.h"
#include "parser/parser.h"
#include "parser/analyze.h"
#include "nodes/print.h"
#include "nodes/makefuncs.h"

#include "catalog/pg_type.h"
#include "catalog/pg_collation.h"

#include "funcapi.h"
#include "miscadmin.h"
#include "nodes/nodeFuncs.h"

#include "utils/syscache.h"
#include "executor/spi_priv.h"
#include "tcop/utility.h"
#include "nodes/readfuncs.h"

#include "optimizer/planner.h"

#include "commands/explain.h"
#include "utils/snapmgr.h"

#include "access/hash.h"

#include "utils/memutils.h"

#ifdef PG_MODULE_MAGIC
PG_MODULE_MAGIC;
#endif

void _PG_init(void);
void _PG_fini(void);

text *format_node(Node *node, bool pretty);
PlannedStmt *injection_planner(Query *parse,
            int cursorOptions,
            ParamListInfo boundParams);

PlannedStmt* myPlan = NULL;

static void query_hash_hook(ParseState *pstate, Query *query);
PlannedStmt *cache_planner(Query *parse, int cursorOptions, ParamListInfo boundParams);

static bool pg_cuckoo_cache_enabled;

PG_FUNCTION_INFO_V1(pg_cuckoo_enable_cache);
Datum pg_cuckoo_enable_cache(PG_FUNCTION_ARGS);
PG_FUNCTION_INFO_V1(pg_cuckoo_disable_cache);
Datum pg_cuckoo_disable_cache(PG_FUNCTION_ARGS);


void _PG_init(void)
{
  DefineCustomBoolVariable("pg_cuckoo.cache",
                            "Enable / Disable pg_cuckoo plan cache",
                            NULL,
                            &pg_cuckoo_cache_enabled,
                            false,
                            PGC_USERSET,
                            0,
                            NULL,
                            NULL,
                            NULL);

  post_parse_analyze_hook = query_hash_hook;
  planner_hook = cache_planner;
}

void _PG_fini(void)
{

}

Datum pg_cuckoo_enable_cache(PG_FUNCTION_ARGS)
{
  (void) set_config_option("pg_cuckoo.cache", "ON", PGC_USERSET, PGC_S_OVERRIDE, GUC_ACTION_SAVE, true, 0, false);
  PG_RETURN_VOID();
}

Datum pg_cuckoo_disable_cache(PG_FUNCTION_ARGS)
{
  (void) set_config_option("pg_cuckoo.cache", "OFF", PGC_USERSET, PGC_S_OVERRIDE, GUC_ACTION_SAVE, true, 0, false);
  PG_RETURN_VOID();
}

static void query_hash_hook(ParseState *pstate, Query *query)
{
  query->queryId = (uint64) DatumGetUInt32(hash_any( (const unsigned char*) pstate->p_sourcetext, strlen(pstate->p_sourcetext)));
}

PlannedStmt *cache_planner(Query *parse, int cursorOptions, ParamListInfo boundParams)
{
  PlannedStmt* planToForce = NULL;
  MemoryContext oldCurr = CurrentMemoryContext;

  // elog(INFO, "cache_planner");
  if(myPlan != NULL)
  {
    return myPlan;
  }

  // elog(INFO, "Query ID: %u", parse->queryId);

  if(!pg_cuckoo_cache_enabled)
  {
    return standard_planner(parse, cursorOptions, boundParams);
  }

  int ret;
  uint64 proc;

  if ((ret = SPI_connect()) < 0)
  /* internal error */
  elog(ERROR, "SPI_connect returned %d", ret);

  // Let SPI prepare a query
  
  Oid argtypes[1] = { INT8OID };
  Datum values[1] = { Int64GetDatum(parse->queryId) };

  planner_hook = NULL;

  ret = SPI_execute_with_args("select * from plan_cache where query_id = $1;"
                              , 1
                              , argtypes
                              , values
                              , NULL
                              , true
                              , 1 );

  // Execution is done at this point, print the result of the query
  proc = SPI_processed; // Number of rows
  // elog(INFO, "executed, rows: %lu", proc);

  /* If no qualifying tuples, fall out early */
  if (ret != SPI_OK_SELECT || proc == 0)
  {
    SPI_finish();
    elog(INFO, "No plan in cache");

    PlannedStmt* plan = standard_planner(parse, cursorOptions, boundParams);

    text* plantext = format_node((Node*) plan, false);

    if ((ret = SPI_connect()) < 0)
    /* internal error */
    elog(ERROR, "SPI_connect returned %d", ret);

    Oid argtypes[3] = { INT8OID, INT8OID, TEXTOID };
    Datum values[3] = { Int64GetDatum(parse->queryId), Int64GetDatum(hash_any( (const unsigned char*) plantext, 64)), CStringGetDatum(plantext) };

    ret = SPI_execute_with_args("INSERT INTO plan_cache VALUES($1,$2,$3,false);"
                               , 3
                               , argtypes
                               , values
                               , NULL
                               , false
                               , 1 );

    planner_hook = &cache_planner;
    SPI_finish();

    return plan;
  }
  else
  {
    elog(INFO, "Found a cache entry");

    TupleDesc tupdesc = SPI_tuptable->tupdesc;
    SPITupleTable *tuptable = SPI_tuptable;
    uint64 j;

    int forceColId = SPI_fnumber(tupdesc, "force_plan");
    int plantextID = SPI_fnumber(tupdesc, "query_plan");

    for (j = 0; j < proc; j++)
    {
      HeapTuple tuple = tuptable->vals[j];

      bool force = false;
      text* plan_string;
      bool isnull = false;
      force = DatumGetBool(SPI_getbinval(tuple, tupdesc, forceColId, &isnull));
      plan_string = DatumGetTextP(SPI_getbinval(tuple, tupdesc, plantextID, &isnull));

      if(force) {
        char *nodeText = text_to_cstring(plan_string);
        elog(INFO, "Should force a plan!");

        // We MUST switch the memory context here. Otherwise planToForce
        // is allocated in the wrong context and PG crashes.
        MemoryContextSwitchTo(oldCurr);

        planToForce = (PlannedStmt*) stringToNode( nodeText );

        // Perform a sanity check.
        if(IsA(planToForce, PlannedStmt)) {
          // planToForce resembles a PlannedStmt, so we are good to go!
          break;
        } else {
          // Something is wrong. Parsing of nodeText did not result in a PlannedStmt.
          elog(INFO, "Error while reconstructing plan");
          planToForce = NULL;
          break;
        }
      }
    }

    planner_hook = &cache_planner;
    SPI_finish();

    if(planToForce == NULL) {
      elog(INFO, "planToForce is null, use standard_planner instead!");
      return standard_planner(parse, cursorOptions, boundParams);
    }

    return planToForce;
  }

  return standard_planner(parse, cursorOptions, boundParams);
}

text *
format_node(Node *node, bool pretty)
{
  if(node == NULL) {
    elog(ERROR, "format_node NULL reference");
  }
  text  *out_t;
  char  *out, *out_f;

  out = nodeToString(node);
  if (pretty) {
      out_f = pretty_format_node_dump(out);
  } 
  else 
  {
      out_f = out;
  }
  out_t = cstring_to_text(out_f);
  return out_t;
}

// As soon as the planner_hook is set, we simply ignore the input from the
// planner and instead return myPlan, which will hold the plan we enforce.
PlannedStmt *injection_planner(Query *parse,
            int cursorOptions,
            ParamListInfo boundParams)
{
  planner_hook=NULL;
  return myPlan;
}

PG_FUNCTION_INFO_V1(pg_plan_serialize);

Datum
pg_plan_serialize(PG_FUNCTION_ARGS)
{
  text  *query_string_t = PG_GETARG_TEXT_P(0);
  bool pretty = PG_GETARG_BOOL(1);
  text  *out;
  char  *query_string;
  List  *parsetree_list;
  RawStmt *parsetree;

  List *querytree;

  query_string = text_to_cstring(query_string_t);

  parsetree_list = raw_parser(query_string);
  // RawStmt node cannot be dumped using nodeToString.
  // We have to access the stmt field.
  parsetree = (RawStmt *) linitial(parsetree_list);

  querytree = pg_analyze_and_rewrite(parsetree, query_string, NULL, 0, NULL);
  
  querytree = pg_plan_queries(querytree, CURSOR_OPT_PARALLEL_OK, NULL);
  out = format_node((Node*) (linitial(querytree)), pretty);
    // out = format_node((Node *) querytree, pretty);

  // out = format_node((Node *) (parsetree->stmt), pretty);
  PG_RETURN_TEXT_P(out);
}

PG_FUNCTION_INFO_V1(pg_plan_execute);

/* This function takes the string representation of a plan and returns the result
   as table valued function.
 */

Datum
pg_plan_execute(PG_FUNCTION_ARGS)
{
  ReturnSetInfo *rsinfo = (ReturnSetInfo *) fcinfo->resultinfo;
  Tuplestorestate *tupstore;
  TupleDesc tupdesc;
  TupleDesc spi_tupdesc;
  AttInMetadata *attinmeta;

  SPITupleTable *tuptable;

  MemoryContext per_query_ctx;
  MemoryContext oldcontext;
  
  text *nodeText = PG_GETARG_TEXT_P(0);
  Node *result;
  text *outputstr;

  SPIPlanPtr res;

  int ret;
  uint64 proc;
  char *nodeChar = text_to_cstring(nodeText);


  /* check to see if caller supports us returning a tuplestore */
  if (rsinfo == NULL || !IsA(rsinfo, ReturnSetInfo))
    ereport(ERROR,
        (errcode(ERRCODE_FEATURE_NOT_SUPPORTED),
         errmsg("set-valued function called in context that cannot accept a set")));
  if (!(rsinfo->allowedModes & SFRM_Materialize))
    ereport(ERROR,
        (errcode(ERRCODE_FEATURE_NOT_SUPPORTED),
         errmsg("materialize mode required, but it is not " \
            "allowed in this context")));

  per_query_ctx = rsinfo->econtext->ecxt_per_query_memory;

  // Deserialize the Plan
  result = (Node*) stringToNode(nodeChar);

  if(!IsA(result, PlannedStmt))
    ereport(ERROR,
          (errcode(ERRCODE_FEATURE_NOT_SUPPORTED),
          errmsg("provided input is not a plan")));

  outputstr = format_node((Node*) result, false);

  // Setup the plan we want to execute
  myPlan = (PlannedStmt*) result;

  // ENGAGE BRAIN HERE

  // <Query-Plan Injection Code>
  // By setting planner_hook here we basically DISABLE the Postgres
  // Planner completely! Instead of giving Postgres the chance to
  // plan whatever the input is, we inject our own plan into the system.
  planner_hook = &injection_planner;

  // Startup a simple SPI connection, which we will abuse to execute
  // our injected plan.
  /* Connect to SPI manager */
  if ((ret = SPI_connect()) < 0)
    /* internal error */
    elog(ERROR, "SPI_connect returned %d", ret);

  // Let SPI prepare a query
  res = SPI_prepare("select 1", 0, NULL);
  //elog(INFO, "prepared");
  
  // Execute the plan.
 ret = SPI_execute_plan(res, NULL, NULL, false, 0);

  // Execution is done at this point, print the result of the query
  proc = SPI_processed; // Number of rows
  // elog(INFO, "executed, rows: %lu", proc);

  /* If no qualifying tuples, fall out early */
  if (ret != SPI_OK_SELECT || proc == 0)
  {
    SPI_finish();
    rsinfo->isDone = ExprEndResult;
    PG_RETURN_NULL();
  }

  spi_tupdesc = SPI_tuptable->tupdesc;
  get_call_result_type(fcinfo, NULL, &tupdesc);
  tuptable = SPI_tuptable;

  /*
   * switch to long-lived memory context
   */
  oldcontext = MemoryContextSwitchTo(per_query_ctx);

  /* make sure we have a persistent copy of the result tupdesc */
  tupdesc = CreateTupleDescCopy(tupdesc);

  /* initialize our tuplestore in long-lived context */
  tupstore =
    tuplestore_begin_heap(rsinfo->allowedModes & SFRM_Materialize_Random,
                false, work_mem);

  MemoryContextSwitchTo(oldcontext);

  attinmeta = TupleDescGetAttInMetadata(tupdesc);

  uint64 j;

  // Fetch result tuples
  for (j = 0; j < proc; j++)
  {
      HeapTuple tuple = tuptable->vals[j];
      tuplestore_puttuple(tupstore, tuple);
      int i;

      for (i = 1; i <= tupdesc->natts; i++)
          SPI_getvalue(tuple, tupdesc, i);
  }

  // elog(INFO, "ret: %u", ret); // Returncode

  rsinfo->returnMode = SFRM_Materialize;
  rsinfo->setResult = tupstore;
  rsinfo->setDesc = tupdesc;

  // Close the SPI connection
  SPI_finish();

  // Disable the hook, and reactivate the planner.
  // Remote plan injection is done at this point.
  planner_hook = NULL;
  // </Query-Plan Injection Code>

  return (Datum) 0;
}

PG_FUNCTION_INFO_V1(pg_plan_execute_print);

Datum
pg_plan_execute_print(PG_FUNCTION_ARGS)
{
  text *nodeText = PG_GETARG_TEXT_P(0);
  Node *result;
  text *outputstr;

  SPIPlanPtr res;

  int ret;
  uint64 proc;
  char *nodeChar = text_to_cstring(nodeText);

  // Deserialize the Plan
  result = (Node*) stringToNode(nodeChar);
  outputstr = format_node((Node*) result, false);

  if(!IsA(result, PlannedStmt))
    ereport(ERROR,
          (errcode(ERRCODE_FEATURE_NOT_SUPPORTED),
          errmsg("provided input is not a plan")));

  // Setup the plan we want to execute
  myPlan = (PlannedStmt*) result;

  // ENGAGE BRAIN HERE

  // <Query-Plan Injection Code>
  // By setting planner_hook here we basically DISABLE the Postgres
  // Planner completely! Instead of giving Postgres the chance to
  // plan whatever the input is, we inject our own plan into the system.
  planner_hook = &injection_planner;

  SPI_connect();

  // Let SPI prepare a query
  res = SPI_prepare("select 1", 0, NULL);
  
  // Execute the plan.
 ret = SPI_execute_plan(res, NULL, NULL, false, 0);

  // Execution is done at this point, print the result of the query
  proc = SPI_processed; // Number of rows
  elog(INFO, "executed, rows: %lu", proc);

  if (ret > 0 && SPI_tuptable != NULL)
  {
      TupleDesc tupdesc = SPI_tuptable->tupdesc;
      SPITupleTable *tuptable = SPI_tuptable;
      char buf[8192];
      uint64 j;

      int i;
      // Print the header
      for (i = 1, buf[0] = 0; i <= tupdesc->natts; i++)
              snprintf(buf + strlen (buf), sizeof(buf) - strlen(buf), " %s%s",
                      SPI_fname(tupdesc, i),
                      (i == tupdesc->natts) ? " " : " |");
          elog(INFO, "EXECQ: %s", buf);

      // Print result rows
      for (j = 0; j < proc; j++)
      {
          HeapTuple tuple = tuptable->vals[j];
          int i;

          for (i = 1, buf[0] = 0; i <= tupdesc->natts; i++)
              snprintf(buf + strlen (buf), sizeof(buf) - strlen(buf), " %s%s",
                      SPI_getvalue(tuple, tupdesc, i),
                      (i == tupdesc->natts) ? " " : " |");
          elog(INFO, "EXECQ: %s", buf);
      }
  }
  else
  {
    // Some error
    elog(INFO, "err");
  }

  elog(INFO, "ret: %u", ret); // Returncode

  // Close the SPI connection
  SPI_finish();

  // Disable the hook, and reactivate the planner.
  // Remote plan injection is done at this point.
  planner_hook = NULL;
  // </Query-Plan Injection Code>

  PG_RETURN_TEXT_P(outputstr);
}

PG_FUNCTION_INFO_V1(pg_plan_explain);

Datum
pg_plan_explain(PG_FUNCTION_ARGS)
{
  text *nodeText = PG_GETARG_TEXT_P(0);
  Node *plan;

  char *nodeChar = text_to_cstring(nodeText);

  // Deserialize the Plan
  plan = (Node*) stringToNode(nodeChar);

  if (IsA(plan, PlannedStmt))
  {
    ExplainState *es = NewExplainState();
    es->costs = false;
    es->verbose = true;
    es->analyze = PG_GETARG_BOOL(1);
    es->summary = es->analyze;
    es->timing = es->analyze;
    ExplainBeginOutput(es);
    PG_TRY();
    {
      ExplainOnePlan((PlannedStmt *)plan, NULL,
             es, "select 1", 
#if PG_VERSION_NUM >= 100000
             NULL, create_queryEnv(), NULL);
#else
             NULL, NULL);
#endif
      PG_RETURN_TEXT_P(cstring_to_text(es->str->data));
    }
    PG_CATCH();
    {
      UnregisterSnapshot(GetActiveSnapshot());
      UnregisterSnapshot(GetActiveSnapshot());
      PopActiveSnapshot();
      ExplainEndOutput(es);
      PG_RETURN_TEXT_P(cstring_to_text("Invalid plan"));
    }
    PG_END_TRY();
    ExplainEndOutput(es);
  }
  else
  {
    PG_RETURN_TEXT_P(cstring_to_text("Input is not a plan"));
  }
}


/**
 * string_to_const
 * 
 * This function takes a `select <foo>` query and tries to
 * extract the parsed and analyzed expression `<foo>`.
 * We can use this to convert any constants we need, into
 * the Query-Tree format.
 */

PG_FUNCTION_INFO_V1(string_to_const);

Datum string_to_const(PG_FUNCTION_ARGS)
{
  text  *query_string_t = PG_GETARG_TEXT_P(0);
  bool pretty = false;
  text  *out;
  char  *query_string;
  List  *parsetree_list;
  RawStmt *parsetree;

  List *querytree;
  Node* constNode = NULL;

  query_string = text_to_cstring(query_string_t);

  parsetree_list = raw_parser(query_string);
  // RawStmt node cannot be dumped using nodeToString.
  // We have to access the stmt field.
  parsetree = (RawStmt *) linitial(parsetree_list);

  querytree = pg_analyze_and_rewrite(parsetree, query_string, NULL, 0, NULL);
  querytree = linitial(querytree);

  if(nodeTag(querytree) == T_Query)
  {
    if(((Query*) querytree)->targetList != NIL) {
      constNode = (Node*) ((TargetEntry*) linitial(((Query*) querytree)->targetList))->expr;
    }
  }

  if(constNode != NULL) {
    out = format_node((Node*) constNode, pretty);
  } else {
    elog(ERROR, "const not found");
  }

  PG_RETURN_TEXT_P(out);
}
