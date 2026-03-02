# ScalaTion Reference Sources for Candidacy Study

This document extracts all `@see` references from ScalaTion source code for ODE integrators and stochastic distributions.

---

## 📚 ODE Integrators (Numerical Methods)

### General References (All Integrators)

| Reference | URL |
|-----------|-----|
| MATLAB ODE Solver Guide | https://www.mathworks.com/help/matlab/math/choose-an-ode-solver.html |
| List of Runge-Kutta Methods | https://en.wikipedia.org/wiki/List_of_Runge%E2%80%93Kutta_methods |
| History of Runge-Kutta (Butcher 1996) | https://people.cs.vt.edu/~asandu/Public/Qual2011/DiffEqn/Butcher_1996_RK-history.pdf |

---

### A. Explicit, Fixed Step-size Methods

| Method | File | Order | Reference |
|--------|------|-------|-----------|
| **Euler** | `RungeKutta2.scala` | 1st | Wikipedia: List of Runge-Kutta Methods |
| **Heun** (Explicit Trapezoidal) | `RungeKutta2.scala` | 2nd | Wikipedia: List of Runge-Kutta Methods |
| **RK2** (Modified Euler / Midpoint) | `RungeKutta2.scala` | 2nd | Wikipedia: List of Runge-Kutta Methods |
| **RK3** (SSPRK3) | `RungeKutta2.scala` | 3rd | Wikipedia: List of Runge-Kutta Methods |
| **RK4** (Classic) | `RungeKutta.scala`, `RungeKutta2.scala` | 4th | Wikipedia: List of Runge-Kutta Methods |
| **RK5** (Butcher's) | `RungeKutta2.scala` | 5th | https://www.researchgate.net/publication/326510985_A_Comparative_Study_on_Fourth_Order_and_Butchers_Fifth_Order_Runge-Kutta_Methods_with_Third_Order_Initial_Value_Problem_IVP |

---

### B. Explicit, Adaptive Step-size (Embedded) Methods

| Method | File | Order | Reference |
|--------|------|-------|-----------|
| **RK23** (Bogacki-Shampine / ode23) | `RungeKutta3.scala` | (2,3) | Wikipedia: List of Runge-Kutta Methods |
| **RK45 / Dormand-Prince** (ode45) | `RungeKutta3.scala`, `DormandPrince.scala` | (4,5) | https://en.wikipedia.org/wiki/Dormand%E2%80%93Prince_method |
| **DormandPrince** (hardcoded) | `DormandPrince.scala` | (4,5) | http://adorio-research.org/wordpress/?p=6565 |

---

### C. Implicit Methods (Stiff ODEs)

| Method | File | Order | Reference |
|--------|------|-------|-----------|
| **ModRosenbrock** (ode23s) | `ModRosenbrock.scala` | (2,3) | https://rotordynamics.wordpress.com/2014/06/18/the-modified-rosenbrock-triple/ |
| **Radau IIA** | `Radau.scala` | - | http://users.bart.nl/users/termaten/Publications/Coached/JdS_Radau_1997.pdf |
| | | | http://www.dm.uniba.it/~testset/solvers/radau5.php |
| | | | https://www.sciencedirect.com/science/article/pii/S0377042797001416 |

---

## 📚 Stochastic Distributions (Random Variates)

### Primary Reference (All Distributions)

| Book | Citation |
|------|----------|
| **Law & Kelton** | Averill M. Law and W. David Kelton, *Simulation Modeling and Analysis*, 2nd Edition, McGraw-Hill, 1991 |

### Online Reference Hub

| Resource | URL |
|----------|-----|
| **UAH Virtual Labs in Statistics** | http://www.math.uah.edu/stat/special/index.html |

---

### Distribution-Specific References

| Distribution | File | Reference URL |
|--------------|------|---------------|
| **Bernoulli** | `Variate.scala` | http://www.math.uah.edu/stat/bernoulli/Introduction.html |
| **Beta** | `Variate.scala` | http://www.math.uah.edu/stat/special/Beta.html |
| **Binomial** | `Variate.scala` | http://www.math.uah.edu/stat/bernoulli/Binomial.html |
| **Cauchy** | `Variate.scala` | http://www.math.uah.edu/stat/special/Cauchy.html |
| **Chi-Square** | `Variate.scala` | http://www.math.uah.edu/stat/special/ChiSquare.html |
| **Erlang** | `Variate.scala` | http://www.math.uah.edu/stat/poisson/Gamma.html |
| **Exponential** | `Variate.scala` | http://www.math.uah.edu/stat/poisson/Exponential.html |
| **Fisher (F)** | `Variate.scala` | http://www.math.uah.edu/stat/special/Fisher.html |
| **Gamma** | `Variate.scala` | http://www.math.uah.edu/stat/poisson/Gamma.html |
| **Geometric** | `Variate.scala` | http://www.math.uah.edu/stat/bernoulli/Geometric.html |
| **Hypergeometric** | `Variate.scala` | http://www.math.uah.edu/stat/urn/Hypergeometric.html |
| **Logistic** | `Variate.scala` | http://www.math.uah.edu/stat/special/Logistic.html |
| **LogNormal** | `Variate.scala` | http://www.math.uah.edu/stat/special/LogNormal.html |
| **Negative Binomial** | `Variate.scala` | http://www.math.uah.edu/stat/bernoulli/NegativeBinomial.html |
| **Normal** | `Variate.scala` | http://www.math.uah.edu/stat/special/Normal.html |
| **Pareto** | `Variate.scala` | http://www.math.uah.edu/stat/special/Pareto.html |
| **Poisson** | `Variate.scala` | http://www.math.uah.edu/stat/poisson/Poisson.html |

---

## 📚 Poisson Process References

| Process | File | Reference |
|---------|------|-----------|
| **Poisson Process** | `random/PoissonProcess.scala` | Law & Kelton (1991) — see above |
| **NH Poisson Process** | `simulation/NH_PoissonProcess.scala` | (No explicit @see — uses thinning method) |
| **Erlang Process** | `simulation/ErlangProcess.scala` | (No explicit @see — k=2 stages) |

---

## 🎯 Study Priority by Exam Question

### Q2: Numerical Integration

| Topic | ScalaTion File | Study Reference |
|-------|----------------|-----------------|
| Euler error analysis | `RungeKutta2.scala` (euler) | Wikipedia + Brunton video |
| RK4 derivation | `RungeKutta.scala` | Wikipedia + Burden & Faires Ch. 5 |
| Dormand-Prince | `DormandPrince.scala` | Wikipedia: Dormand-Prince |
| Butcher Tableau | `RungeKutta2.scala`, `RungeKutta3.scala` | Butcher 1996 PDF |
| Stiff ODEs | `ModRosenbrock.scala`, `Radau.scala` | Rotordynamics blog |

### Q3: Stochastic Processes

| Topic | ScalaTion File | Study Reference |
|-------|----------------|-----------------|
| Poisson distribution | `Variate.scala` (Poisson) | UAH: http://www.math.uah.edu/stat/poisson/Poisson.html |
| Exponential distribution | `Variate.scala` (Exponential) | UAH: http://www.math.uah.edu/stat/poisson/Exponential.html |
| Erlang distribution | `Variate.scala` (Erlang) | UAH: http://www.math.uah.edu/stat/poisson/Gamma.html |
| Erlang-2 shifted | `Variate.scala` (Erlang2S) | (No @see — your ANNSIM contribution!) |
| Poisson Process | `PoissonProcess.scala` | Law & Kelton Ch. 6 |

---

## 📖 How to Study Using These References

### Step 1: Read the ScalaTion Code
```
src/main/scala/scalation/dynamics/RungeKutta2.scala   → Butcher Tableau for RK methods
src/main/scala/scalation/dynamics/DormandPrince.scala → Adaptive step-size
src/main/scala/scalation/random/Variate.scala         → All distributions
```

### Step 2: Follow the @see Links
Each link explains the mathematical derivation. The UAH Virtual Labs are especially good — they include:
- PDF/PMF formulas
- Mean and variance derivations
- Parameter relationships
- Interactive simulations

### Step 3: Cross-Reference with Your Papers
- **ANNSIM 2026**: Why Erlang-2 (CV = 0.707) beats Exponential (CV = 1.0)
- **WSC 2026**: How integrator choice affects car-following calibration

---

## 🔗 Quick Links for Your Exam

### Numerical ODE Methods
1. https://en.wikipedia.org/wiki/List_of_Runge%E2%80%93Kutta_methods
2. https://en.wikipedia.org/wiki/Dormand%E2%80%93Prince_method
3. https://www.mathworks.com/help/matlab/math/choose-an-ode-solver.html

### Stochastic Distributions
1. http://www.math.uah.edu/stat/poisson/Poisson.html (Poisson)
2. http://www.math.uah.edu/stat/poisson/Exponential.html (Exponential)
3. http://www.math.uah.edu/stat/poisson/Gamma.html (Erlang/Gamma)

### Textbook
- **Law & Kelton** — *Simulation Modeling and Analysis* (covers both ODE integration and stochastic processes for simulation)

---

## 📁 File Locations in ScalaTion

```
src/main/scala/scalation/
├── dynamics/
│   ├── Integrator.scala        # Base trait
│   ├── RungeKutta.scala        # RK4 (hardcoded)
│   ├── RungeKutta2.scala       # Euler, Heun, RK2, RK3, RK4, RK5
│   ├── RungeKutta3.scala       # RK23, RK45 (adaptive)
│   ├── DormandPrince.scala     # DOPRI5 (hardcoded)
│   ├── ModRosenbrock.scala     # Stiff ODE solver
│   ├── Radau.scala             # Implicit Runge-Kutta
│   └── integrators.txt         # Summary of all integrators
├── random/
│   ├── Variate.scala           # All distributions (30+)
│   └── PoissonProcess.scala    # Time-varying Poisson
└── simulation/
    ├── PoissonProcess.scala    # Simple Poisson arrivals
    ├── NH_PoissonProcess.scala # Non-homogeneous Poisson
    └── ErlangProcess.scala     # Erlang arrivals (k=2)
```

