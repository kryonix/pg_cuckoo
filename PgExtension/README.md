# pg_cuckoo

pg_cuckoo is a PostgreSQL extension that provides plan injections using the `planner_hook`. 

Note: This extension is not production ready.

# Plan Sources

A plan source is required to use PgCuckoo.
One of these sources can be PostgreSQL itself.
For an arbitrary Query Q, `SELECT plan_serialize('Q')` will return the textual representation of _the_ plan PostgreSQL came up with.

For example using PostgreSQL 10, `SELECT plan_serialize('SELECT 1', false);` will return the following string:
```
{PLANNEDSTMT :commandType 1 :queryId 0 :hasReturning false :hasModifyingCTE false :canSetTag true :transientPlan false :dependsOnRole false :parallelModeNeeded false :planTree {RESULT :startup_cost 0.00 :total_cost 0.01 :plan_rows 1 :plan_width 4 :parallel_aware false :parallel_safe true :plan_node_id 0 :targetlist ({TARGETENTRY :expr {CONST :consttype 23 :consttypmod -1 :constcollid 0 :constlen 4 :constbyval true :constisnull false :location 7 :constvalue 4 [ 1 0 0 0 0 0 0 0 ]} :resno 1 :resname ?column? :ressortgroupref 0 :resorigtbl 0 :resorigcol 0 :resjunk false}) :qual <> :lefttree <> :righttree <> :initPlan <> :extParam (b) :allParam (b) :resconstantqual <>} :rtable <> :resultRelations <> :nonleafResultRelations <> :rootResultRelations <> :subplans <> :rewindPlanIDs (b) :rowMarks <> :relationOids <> :invalItems <> :nParamExec 0 :utilityStmt <> :stmt_location 0 :stmt_len 0}
```

To execute this plan, `plan_execute('...')` is required.

`SELECT * FROM plan_execute('{PLANNEDSTMT :commandType 1 :queryId 0 :hasReturning false :hasModifyingCTE false :canSetTag true :transientPlan false :dependsOnRole false :parallelModeNeeded false :planTree {RESULT :startup_cost 0.00 :total_cost 0.01 :plan_rows 1 :plan_width 4 :parallel_aware false :parallel_safe true :plan_node_id 0 :targetlist ({TARGETENTRY :expr {CONST :consttype 23 :consttypmod -1 :constcollid 0 :constlen 4 :constbyval true :constisnull false :location 7 :constvalue 4 [ 1 0 0 0 0 0 0 0 ]} :resno 1 :resname ?column? :ressortgroupref 0 :resorigtbl 0 :resorigcol 0 :resjunk false}) :qual <> :lefttree <> :righttree <> :initPlan <> :extParam (b) :allParam (b) :resconstantqual <>} :rtable <> :resultRelations <> :nonleafResultRelations <> :rootResultRelations <> :subplans <> :rewindPlanIDs (b) :rowMarks <> :relationOids <> :invalItems <> :nParamExec 0 :utilityStmt <> :stmt_location 0 :stmt_len 0}') AS result(col1 int4)`

`plan_execute` is a table valued function that takes the textual representation of a PostgreSQL plan and executes it. The return type of this function is polymorphic (`SETOF RECORD`) which requires the use of a column definition list.

Using this textual representation provides full control over execution plans.

In this example we took the plan from above and added another column.

`SELECT * FROM plan_execute('{PLANNEDSTMT :commandType 1 :queryId 0 :hasReturning false :hasModifyingCTE false :canSetTag true :transientPlan false :dependsOnRole false :parallelModeNeeded false :planTree {RESULT :startup_cost 0.00 :total_cost 0.01 :plan_rows 1 :plan_width 4 :parallel_aware false :parallel_safe true :plan_node_id 0 :targetlist ({TARGETENTRY :expr {CONST :consttype 23 :consttypmod -1 :constcollid 0 :constlen 4 :constbyval true :constisnull false :location 7 :constvalue 4 [ 1 0 0 0 0 0 0 0 ]} :resno 1 :resname ?column? :ressortgroupref 0 :resorigtbl 0 :resorigcol 0 :resjunk false} {TARGETENTRY :expr {CONST :consttype 23 :consttypmod -1 :constcollid 0 :constlen 4 :constbyval true :constisnull false :location 7 :constvalue 4 [ 42 0 0 0 0 0 0 0 ]} :resno 2 :resname ?column? :ressortgroupref 0 :resorigtbl 0 :resorigcol 0 :resjunk false}) :qual <> :lefttree <> :righttree <> :initPlan <> :extParam (b) :allParam (b) :resconstantqual <>} :rtable <> :resultRelations <> :nonleafResultRelations <> :rootResultRelations <> :subplans <> :rewindPlanIDs (b) :rowMarks <> :relationOids <> :invalItems <> :nParamExec 0 :utilityStmt <> :stmt_location 0 :stmt_len 0}') AS result(col1 int4, col2 int4)`

## Pretty Printing

It is also possible to get a slightly prettier version of the query plans by using `SELECT plan_serialize('Q', TRUE)`:

```
{PLANNEDSTMT
:commandType 1
:queryId 3159486701
:hasReturning false
:hasModifyingCTE false
:canSetTag true
:transientPlan false
:dependsOnRole false
:parallelModeNeeded false
:planTree
   {RESULT
   :startup_cost 0.00
   :total_cost 0.01
   :plan_rows 1
   :plan_width 4
   :parallel_aware false
   :parallel_safe true
   :plan_node_id 0
   :targetlist (
      {TARGETENTRY
      :expr
         {CONST
         :consttype 23
         :consttypmod -1
         :constcollid 0
         :constlen 4
         :constbyval true
         :constisnull false
         :location 7
         :constvalue 4 [ 1 0 0 0 0 0 0 0 ]
         }
      :resno 1
      :resname ?column?
      :ressortgroupref 0
      :resorigtbl 0
      :resorigcol 0
      :resjunk false
      }
   )
   :qual <>
   :lefttree <>
   :righttree <>
   :initPlan <>
   :extParam (b)
   :allParam (b)
   :resconstantqual <>
   }
:rtable <>
:resultRelations <>
:nonleafResultRelations <>
:rootResultRelations <>
:subplans <>
:rewindPlanIDs (b)
:rowMarks <>
:relationOids <>
:invalItems <>
:nParamExec 0
:utilityStmt <>
:stmt_location 0
:stmt_len 0
}
```

Note: this form is not executable using `plan_execute`.

# Applications

https://dbworld.informatik.uni-tuebingen.de/publications/PgCuckoo-LayingPlanEggsIntoPostgreSQL-sNest.html

## Plan Forcing
TBA

## Plan Stitching
TBA

## Query Cache
TBA


# Installation

## Requirements

- PostgreSQL ⩾ 10 (PG11 is not supported yet!)

## Operations

```
$ git clone https://github.com/kryonix/pg_cuckoo.git pg_cuckoo

$ cd pg_cuckoo/src
$ make && make install

$ psql -c "CREATE EXTENSION cuckoo"
```

# Support
Please include the following information when reporting a problem with pg_cuckoo:

- PostgreSQL Version
- Query
- DDL Statements (CREATE TABLE, CREATE INDEX etc.)

We will use this information to reproduce your problem locally.

# Author

Denis Hirn

Copyright (c) 2018-2019, Denis Hirn
