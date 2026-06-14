// SPDX-License-Identifier: MPL-2.0
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for VeriSimError.affine (idaptik VeriSimDB client error
// classification; scalar i32 ABI). The oracle re-derives fromStatus and
// isRetryable from VeriSimError.res semantics in plain JS, keyed by the variant
// declaration ordinal, so a codegen regression surfaces as a differential
// mismatch.
//
// Category ordinals (VeriSimError.res variant declaration order):
//   0 BadRequest 1 Unauthorized 2 Forbidden 3 NotFound 4 Conflict
//   5 ValidationFailed 6 RateLimited 7 InternalError 8 ServiceUnavailable
//   9 HexadNotFound 10 ModalityUnavailable 11 DriftComputationError
//   12 ProvenanceInvalid 13 VclParseError 14 VclExecutionError 15 FederationError
//   16 ConnectionError 17 TimeoutError 18 SerializationError 19 UnknownError

// Independent re-implementation of VeriSimError.res fromStatus, as a status->ord
// lookup. Any status not in the table is UnknownError (19), matching the
// `code => UnknownError(...)` catch-all arm.
const STATUS_TO_ORD = {
  400: 0,
  401: 1,
  403: 2,
  404: 3,
  409: 4,
  422: 5,
  429: 6,
  500: 7,
  503: 8,
};
const classifyStatus = (status) =>
  Object.prototype.hasOwnProperty.call(STATUS_TO_ORD, status)
    ? STATUS_TO_ORD[status]
    : 19;

// Independent re-implementation of VeriSimError.res isRetryable, as the set of
// retryable category ordinals: RateLimited, InternalError, ServiceUnavailable,
// ConnectionError, TimeoutError.
const RETRYABLE = new Set([6, 7, 8, 16, 17]);
const validCategory = (ord) => ord >= 0 && ord <= 19;
const isCategoryRetryable = (ord) =>
  validCategory(ord) && RETRYABLE.has(ord) ? 1 : 0;
const isStatusRetryable = (status) => isCategoryRetryable(classifyStatus(status));

export default {
  affine: "VeriSimError.affine",
  cases: [
    {
      name: "error_category_count()",
      export: "error_category_count",
      args: [],
      oracle: () => 20,
    },
    {
      name: "is_valid_error_category over [-3..25]",
      export: "is_valid_error_category",
      args: [[-3, 25]],
      oracle: (ord) => (validCategory(ord) ? 1 : 0),
    },
    {
      // Sweep the whole 4xx/5xx band plus unmapped codes (e.g. 200, 418, 599)
      // so both the nine hits and the UnknownError catch-all are exercised.
      name: "classify_status over [200..599]",
      export: "classify_status",
      args: [[200, 599]],
      oracle: classifyStatus,
    },
    {
      name: "is_category_retryable over categories [-3..25]",
      export: "is_category_retryable",
      args: [[-3, 25]],
      oracle: isCategoryRetryable,
    },
    {
      name: "is_status_retryable over [200..599]",
      export: "is_status_retryable",
      args: [[200, 599]],
      oracle: isStatusRetryable,
    },
  ],
};
