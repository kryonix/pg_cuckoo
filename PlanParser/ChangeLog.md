# Changelog for PlanParser

## Unreleased changes
- Add binding parameter to dependent join operator. Used to indicate which parameters a dep-join binds.
- Leave PARAMs in the AST
- TODO:
  + Free-Variable detection should be obsolete using params
  + De-ref PARAM when dep join is pushed down
