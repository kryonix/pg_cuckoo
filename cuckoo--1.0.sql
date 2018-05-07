SET search_path = public;

CREATE OR REPLACE FUNCTION _pq_plan_serialize(query_string text, pretty_print bool DEFAULT true)
RETURNS text
AS '$libdir/cuckoo','pq_plan_serialize'
LANGUAGE C IMMUTABLE STRICT;

CREATE OR REPLACE FUNCTION _pq_plan_deserialize(query_string text, pretty_print bool DEFAULT true)
RETURNS text
AS '$libdir/cuckoo','pq_plan_deserialize'
LANGUAGE C IMMUTABLE STRICT;

CREATE OR REPLACE FUNCTION _pq_plan_deserialize2(query_string text, pretty_print bool DEFAULT true)
RETURNS setof record
AS '$libdir/cuckoo','pq_plan_deserialize2'
LANGUAGE C IMMUTABLE STRICT;

CREATE OR REPLACE FUNCTION _pq_plan_explain(query_string text, plan_analyze bool DEFAULT false)
RETURNS text
AS '$libdir/cuckoo','pq_plan_explain'
LANGUAGE C IMMUTABLE STRICT;

CREATE OR REPLACE FUNCTION string_to_const(const_string text)
RETURNS text
AS '$libdir/cuckoo','string_to_const'
LANGUAGE C IMMUTABLE STRICT;

CREATE FUNCTION plan_serialize(query_string text, pretty_print bool DEFAULT true)
RETURNS text
AS $$
BEGIN
  RETURN (SELECT @extschema@._pq_plan_serialize(query_string, pretty_print));
END;
$$ LANGUAGE plpgsql STRICT;

CREATE FUNCTION plan_deserialize(plan_string text)
RETURNS text
AS $$
BEGIN
  RETURN (SELECT @extschema@._pq_plan_deserialize(plan_string));
END;
$$ LANGUAGE plpgsql STRICT;

CREATE FUNCTION plan_deserialize2(plan_string text)
RETURNS setof record
AS $$
BEGIN
  RETURN QUERY (SELECT @extschema@._pq_plan_deserialize2(plan_string));
END;
$$ LANGUAGE plpgsql;
