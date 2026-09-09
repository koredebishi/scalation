# Repository Working Rules

These instructions apply to all work in this repository.

## Reuse Scalation Before Implementing Anything New

- Before implementing any data structure, collector, statistic, report, utility, abstraction, or helper, search the entire Scalation repository for an existing native implementation. Search repo-wide, not only in the current package or feature directory.
- Identify the existing candidates and explain why they do or do not satisfy the requirement before proposing a new implementation.
- Prefer adapting, extending, composing, or correctly using an existing Scalation class over introducing a parallel implementation.
- Do not create a scratch, substitute, or duplicate data structure unless the user explicitly requests or approves it after reviewing why no existing Scalation implementation is suitable.

## Justify Ownership Before Editing

- Before making a change, ask why the change is needed and which class properly owns the responsibility.
- Do not place feature-specific state or reporting inside a broad class merely because it is convenient. For example, `Model` is the simulation model/director; do not add a parallel `BasicStats` system there when Scalation already has `Statistic`, `TimeStatistic`, `getStatistics`, `report`, and `reportV`.
- Use the established statistics/reporting system as the source of truth. Derive feature-specific measures from those statistics and the minimum unique domain inputs (for example, deriving density from the appropriate statistic and link length) instead of maintaining duplicate counters or reports.

## Approval for Cross-Cutting Changes

- Before touching `Model`, `Component`, or any class the user has not already been told will change, show the user the complete list of affected classes/files and describe the intended change to each one.
- Wait for the user's explicit approval of that scope before editing those classes.
- If the scope expands during the work, stop and disclose the additional classes and intended changes before touching them.

## Builds and Execution

- Do not compile this repository for the user.
- Do not run build, test, benchmark, or simulation commands unless the user explicitly requests that exact execution. The user performs compilation by default.
- A message such as "continue" authorizes continued discussion or analysis only. It does not authorize code changes, compilation, tests, benchmarks, or simulations.

