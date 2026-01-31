# Erlang2S Formula Verification

## Code Facts

**Variate.scala line 475:**
```scala
def gen: Double = tau - mu * log (r.gen * r.gen)
```

**Variate.scala line 470:**
```scala
val mean = tau + 2 * mu
```

**VSource.scala line 419:**
```scala
val muPerStage = (mu - erlang2S.tau) / 2.0
```

## Paper Equations

**Eq 1:** $Y = \tau - \mu \ln(U_1 U_2)$

**Eq 2:** $\mu = \frac{\bar{\mu}_{\ell,t} - \tau}{2}$

## Claims to Verify

1. Is $E = -\mu \ln(U)$ valid for exponential with mean $\mu$, where $U \sim \text{Uniform}(0,1)$?
2. If $X = E_1 + E_2$ (independent exponentials, mean $\mu$ each), is $X$ Erlang-2 with mean $2\mu$?
3. Does $-\mu \ln(U_1) - \mu \ln(U_2) = -\mu \ln(U_1 U_2)$?
4. If $Y = \tau + X$ and $E[X] = 2\mu$, is $E[Y] = \tau + 2\mu$?
5. Solving $\bar{\mu} = \tau + 2\mu$ for $\mu$ gives $\mu = (\bar{\mu} - \tau)/2$?

**All 5 must be YES.**
