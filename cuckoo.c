#include "postgres.h"

#include "fmgr.h"
#include "utils/builtins.h"
#include "parser/parser.h"
#include "parser/analyze.h"
// #include "tcop/tcopprot.h"
#include "nodes/print.h"
#include "nodes/makefuncs.h"

// #include "access/htup_details.h"
// #include "access/xact.h"
// #include "catalog/dependency.h"
// #include "catalog/indexing.h"
// #include "catalog/objectaccess.h"
// #include "catalog/pg_language.h"
// #include "catalog/pg_namespace.h"

// #include "catalog/pg_proc.h"

// #include "catalog/pg_proc_fn.h"
// #include "catalog/pg_transform.h"

#include "catalog/pg_type.h"
#include "catalog/pg_collation.h"

// #include "commands/defrem.h"

// #include "executor/functions.h"

#include "funcapi.h"
// #include "mb/pg_wchar.h"
#include "miscadmin.h"
#include "nodes/nodeFuncs.h"
// #include "parser/parse_type.h"
// #include "tcop/pquery.h"
// #include "utils/acl.h"
// #include "utils/lsyscache.h"
// #include "utils/rel.h"

#include "utils/syscache.h"

// #include "lib/stringinfo.h"

#include "executor/spi_priv.h"

// #include "parser/parse_coerce.h"
#include "tcop/utility.h"
// #include "tsearch/ts_locale.h"

// #include "utils/guc.h"
// #include "utils/memutils.h"
// #include "utils/typcache.h"
// #include "utils/varlena.h"

#include "nodes/readfuncs.h"


// #include "pgstat.h"

#include "optimizer/planner.h"

#include "commands/explain.h"
#include "utils/snapmgr.h"

#ifdef PG_MODULE_MAGIC
PG_MODULE_MAGIC;
#endif

text *format_node(Node *node, bool pretty);

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


PlannedStmt *sr_planner(Query *parse,
            int cursorOptions,
            ParamListInfo boundParams);

PlannedStmt* myEvilPlan = NULL;

// As soon as the planner_hook is set, we simply ignore the input from the
// planner and instead return myEvilPlan, which will hold the plan we enforce.
PlannedStmt *sr_planner(Query *parse,
            int cursorOptions,
            ParamListInfo boundParams)
{
  return myEvilPlan;
}

PG_FUNCTION_INFO_V1(pq_plan_serialize);

Datum
pq_plan_serialize(PG_FUNCTION_ARGS)
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
  out = format_node((Node*) querytree, pretty);
    // out = format_node((Node *) querytree, pretty);

  // out = format_node((Node *) (parsetree->stmt), pretty);
  PG_RETURN_TEXT_P(out);
}

PG_FUNCTION_INFO_V1(pq_plan_deserialize2);

Datum
pq_plan_deserialize2(PG_FUNCTION_ARGS)
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
  outputstr = format_node((Node*) result, false);

  // Setup the plan we want to execute
  myEvilPlan = (PlannedStmt*) result;

  // ENGAGE BRAIN HERE

  // <Query-Plan Injection Code>
  // By setting planner_hook here we basically DISABLE the Postgres
  // Planner completely! Instead of giving Postgres the chance to
  // plan whatever the input is, we inject our own plan into the system.
  planner_hook = &sr_planner;

  // Startup a simple SPI connection, which we will abuse to execute
  // our 'evil plan'.
  /* Connect to SPI manager */
  if ((ret = SPI_connect()) < 0)
    /* internal error */
    elog(ERROR, "SPI_connect returned %d", ret);


  // Let SPI prepare a query
  // We might just replace SPI_prepare and SPI_execute_plan by SPI_execute
  // in the future.
  res = SPI_prepare("select 1", 0, NULL);
  elog(INFO, "prepared");
  
  // Execute the plan.
 ret = SPI_execute_plan(res, NULL, NULL, false, 0);

  // Execution is done at this point, print the result of the query
  proc = SPI_processed; // Number of rows
  elog(INFO, "executed, rows: %lu", proc);

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
      tuplestore_puttuple(tupstore, tuple);
      int i;

      for (i = 1, buf[0] = 0; i <= tupdesc->natts; i++)
          snprintf(buf + strlen (buf), sizeof(buf) - strlen(buf), " %s%s",
                  SPI_getvalue(tuple, tupdesc, i),
                  (i == tupdesc->natts) ? " " : " |");
      elog(INFO, "EXECQ: %s", buf);
  }

  elog(INFO, "ret: %u", ret); // Returncode

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
//  PG_RETURN_TEXT_P(outputstr);
}

PG_FUNCTION_INFO_V1(pq_plan_deserialize);

Datum
pq_plan_deserialize(PG_FUNCTION_ARGS)
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

  // Setup the plan we want to execute
  myEvilPlan = (PlannedStmt*) result;

  // ENGAGE BRAIN HERE

  // <Query-Plan Injection Code>
  // By setting planner_hook here we basically DISABLE the Postgres
  // Planner completely! Instead of giving Postgres the chance to
  // plan whatever the input is, we inject our own plan into the system.
  planner_hook = &sr_planner;

  // Startup a simple SPI connection, which we will abuse to execute
  // our 'evil plan'.
  SPI_connect();

  // Let SPI prepare a query
  // We might just replace SPI_prepare and SPI_execute_plan by SPI_execute
  // in the future.
  res = SPI_prepare("select 1", 0, NULL);
  elog(INFO, "prepared");
  
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

PG_FUNCTION_INFO_V1(pq_plan_explain);

Datum
pq_plan_explain(PG_FUNCTION_ARGS)
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
      /* Magic hack but work. In ExplainOnePlan we twice touched snapshot before die.*/
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
    PG_RETURN_TEXT_P(cstring_to_text("Not found plan"));
  }
}

// AST-Node generators

static Datum string_to_datum (const char *str, Oid datatype);
static Datum string_to_datum  ( const char *str,
                                Oid datatype)
{
    Assert(str != NULL);

    /*
     * We cheat a little by assuming that CStringGetTextDatum() will do for
     * bpchar and varchar constants too...
     */
    if (datatype == NAMEOID)
        return DirectFunctionCall1(namein, CStringGetDatum(str));
    else if (datatype == BYTEAOID)
        return DirectFunctionCall1(byteain, CStringGetDatum(str));
    else
        return CStringGetTextDatum(str);
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

// Datum string_to_const(PG_FUNCTION_ARGS)
// {
//   text *inputText = PG_GETARG_TEXT_P(0);
//   char *inputChar = text_to_cstring(inputText);
//   Oid  datatype   = PG_GETARG_OID(1);

//   Const *constNode;
//   text  *out;

//   Datum       conval = string_to_datum(inputChar, datatype);
//   Oid         collation;
//   int         constlen;

//   /*
//   * We only need to support a few datatypes here, so hard-wire properties
//   * instead of incurring the expense of catalog lookups.
//   */
//   switch (datatype)
//   {
//      case TEXTOID:
//      case VARCHAROID:
//      case BPCHAROID:
//          collation = DEFAULT_COLLATION_OID;
//          constlen = -1;
//          break;

//      case NAMEOID:
//          collation = InvalidOid;
//          constlen = NAMEDATALEN;
//          break;

//      case BYTEAOID:
//          collation = InvalidOid;
//          constlen = -1;
//          break;

//      default:
//          elog(ERROR, "unexpected datatype in string_to_const: %u",
//               datatype);
//   }

//   constNode = makeConst(datatype, -1, collation, constlen,
//                         conval, false, false);

//   out = format_node((Node*) constNode, false);
//   PG_RETURN_TEXT_P(out);
// }
