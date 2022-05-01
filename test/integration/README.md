Integration Tests for the Instana Haskell Trace SDK
===================================================

The tests in this folder start an application under test written in Haskell
(see `test/apps`), that is using the Instana Haskell Trace SDK internally,
connecting to a separate process that emulates the behavior of the Instana agent
(see `test/agent-stub`).

Test Organization
-----------------

The test suite modules are located in `test/Instana/SDK/IntegrationTest`. Besides
the modules that contain test cases, there are a number of auxiliary modules.
All test cases that are to be executed need to be listed in `TestSuites.hs`,
where they are bundled into suites. Test cases from any module can be referred
to either individually or by using the `allTests` functions from a module with
multiple test cases. Which style is chose usually depends on whether an
individual test case needs a particular setup (agent stub behavior etc.)

Running Suites and Tests Exclusively
------------------------------------

All test suites in `TestSuites.hs` are declared with a type from
`Instana.SDK.IntegrationTest.Suite.ConditionalSuite`:

```
data ConditionalSuite =
    Run Suite
  | Exclusive Suite
  | Skip Suite
```

Usually, all suites are declared with `Run`. If you want to skip a particular
test suite, change `Run` to `Skip` temporarily. If you want to run one or
multiple suites exclusively and skip all other suites, change `Run` to
`Exclusive` for the suites in question.

Individual test cases are prefixed with
`Instana.SDK.IntegrationTest.HUnitExtra.applyLabel`.  If you want to skip
individual test cases that are part of a suite, swap out `applyLabel`
temporarily with `Instana.SDK.IntegrationTest.HUnitExtra.skip`. There is
currently no mechanism to run individual test cases in a suite exclusively, that
is, the way to achieve that is to skip all other test cases.

Make sure to not accidentally commit changes that skip existing test cases or
suites or switch test suites being run exclusively.
