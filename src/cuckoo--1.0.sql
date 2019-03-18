SET search_path = public;

CREATE OR REPLACE FUNCTION plan_serialize(query_string text, pretty_print bool DEFAULT true)
RETURNS text
AS '$libdir/cuckoo','pg_plan_serialize'
LANGUAGE C IMMUTABLE STRICT;

CREATE OR REPLACE FUNCTION plan_execute_print(query_string text, pretty_print bool DEFAULT true)
RETURNS text
AS '$libdir/cuckoo','pg_plan_execute_print'
LANGUAGE C IMMUTABLE STRICT;

CREATE OR REPLACE FUNCTION plan_execute(query_string text, pretty_print bool DEFAULT true)
RETURNS setof record
AS '$libdir/cuckoo','pg_plan_execute'
LANGUAGE C IMMUTABLE STRICT;

CREATE OR REPLACE FUNCTION plan_explain(query_string text, plan_analyze bool DEFAULT false)
RETURNS text
AS '$libdir/cuckoo','pg_plan_explain'
LANGUAGE C IMMUTABLE STRICT;

CREATE OR REPLACE FUNCTION string_to_const(const_string text)
RETURNS text
AS '$libdir/cuckoo','string_to_const'
LANGUAGE C IMMUTABLE STRICT;
