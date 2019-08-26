# PgCuckoo

PgCuckoo significantly alters PostgreSQL’s operation: we use the _planner hook_ to inject query plan trees from outside the system and have these foreign plans be executed by the system's  query executor.

## PostgreSQL Extension

PostgreSQL's _planner hook_ can be used to open a side entrance through which we can pass _plan trees_ for immediate execution.

The [PostgreSQL extension](PgExtension/) makes this functionality available at the surface language level in terms of a table-valued SQL function `plan_execute(·)` whose argument contains a textual representation of the plan piece. 


## PgCuckoo Plan Decorator

[PgCuckoo](PgCuckoo/) provides a Haskell based plan decorator through which code generators may route skeleton plan trees before they are passed into PostgreSQL. The decorator completes the skeleton through rule-based property inference and queries PostgreSQL’s catalog to, e.g., infer expression types, resolve operator overloading, and establish node-to-node references.
This ensures that externally crafted trees perfectly mimic regular plans.


1. [PgCuckoo: Laying Plan Eggs in PostgreSQL’s Nest. Proc. SIGMOD 2019](https://db.inf.uni-tuebingen.de/staticfiles/publications/pgcuckoo-laying-plan-eggs.pdf)
2. [PgCuckoo – Injecting Physical Plans into PostgreSQL. Proc. BTW 2019](https://db.inf.uni-tuebingen.de/staticfiles/publications/pgcuckoo.pdf)