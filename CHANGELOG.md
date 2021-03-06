# Changelog for instana-haskell-trace-sdk

## Unreleased
- Breaking: Rename `Instana.SDK.SDK.currentTraceId` (with return type `Instana.SDK.Internal.Id.Id`) to `currentTraceIdInternal`. The function `Instana.SDK.SDK.currentTraceId` returns type `String` now.
- Provide new convenience accessors `Instana.SDK.SDK`:
    - `currentSpan` (provides the currently active span in a simplified format),
    - `currentTraceId` (provides the trace ID of the currently active trace),
    - `currentSpanId` (provides the span ID of the currently active span), and
    - `currentParentId` (provides the parent ID of the currently active span).
- Remove deprecated attribute `span.ta`.
- Fix: Pass span ID of exit span downstream with X-INSTANA-S instead of the entry span's span ID.
- Capture HTTP status code in `withCorrelatedHttpEntry`.

## 0.5.0.1
- No changes, only documentation updates.

## 0.5.0.0
- Add support for website monitoring back end correlation via Server-Timing.
- Add support for website monitoring back end correlation via X-INSTANA-L/correlationType/correlationId.

## 0.4.0.0
- Accomodate for breaking changes in `network-3.0.0.0`.

## 0.3.0.0
- Honor the environment variable `INSTANA_SERVICE_NAME` to override the default service name in Instana.
- Add a configuration option for overriding the default service name in Instana.
- Add `InstanaSDK.setServiceName` to override the default service name in Instana on a per-call basis.
- Fix: Send correct SDK spans. This is a breaking change. Several functions were renamed or have changed their signature.
In detail:

    - Use `InstanaSDK.addTag` instead of `InstanaSDK.addData` (for SDK
      spans).
    - Use `InstanaSDK.addTagAt` instead of `InstanaSDK.addDataAt` (for SDK spans).
    - For registered spans, replace `InstanaSDK.addData` and `InstanaSDK.addDataAt` with `InstanaSDK.addRegisteredData` and `InstanaSDK.addRegisteredDataAt`. Note that you should probably not create registered spans, but only use SDK spans.
    - Usages like `startEntry "some.span.name"` or `withEntry "some.span.name"` (that is, the span name is passed directly as a literal) will simply continue to work as expected but will require `OverloadedStrings` to be active.
    - Usages where the span name is stored in a `Data.Text` value first and then passed to `startEntry`/`withEntry`/etc. will break. You can fix those by importing
```
import qualified Instana.SDK.Span.SpanType as SpanType
```
and then wrapping the span name in `SpanType.SdkSpan`. For example:
```
spanName = T.pack "some.span.name"
InstanaSDK.withRootEntry instana spanName ...
```
becomes:
```
spanType = SpanType.SdkSpan $ T.pack "some.span.name"
InstanaSDK.withRootEntry instana spanType ...
```

## 0.2.0.0
- Add WAI middleware plug-in to trace HTTP entries automatically.

## 0.1.0.0
- Initial release
