# pg_cuckoo

pg_cuckoo is a PostgreSQL extension that provides plan injections using the `planner_hook`. 

Note: This extension is not production ready.



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

Denis Hirn (@kryonix)

Copyright (c) 2018-2019, Denis Hirn
