# Skill: Read This Paper

When triggered, read and summarize a research paper with structured analysis.

## Trigger Phrases
- "Read this paper"
- "Summarize this paper"
- "What does this paper do?"
- "Tell me about this paper"

## Output Requirements

### 1. Chat Reply (~30-40 lines)

Provide a structured summary in this exact format:

```
## 📄 [Paper Title]
**Authors:** [Names] | **Venue:** [Conference/Journal] | **Year:** [YYYY]

### 🎯 One-Line Summary
[Single sentence capturing the core contribution]

### 🏷️ Paper Type & Context
[Classify and contextualize the paper. Examples:]
- "This is a GNN paper using attention-based message passing"
- "This is a transformer paper that applies self-attention to traffic prediction"
- "This is a classical car-following model paper (no ML)"
- "This is an RL paper using policy gradients for signal control"
- "This is a hybrid: combines [A] with [B] via [method]"

### 🔑 Key Contributions (3-5 bullets)
- [Contribution 1]
- [Contribution 2]
- [Contribution 3]

### 📊 Dataset(s)
| Dataset | Domain | Why Used | Familiarity |
|---------|--------|----------|-------------|
| [Name] | [Traffic/Graph/etc] | [Their reason] | ⭐ Common / 🆕 New to me |

[If dataset is common: "This is one of the standard benchmarks in [field]"]
[If dataset is new: "This dataset is new to me - worth investigating"]

### 🛠️ Method Summary
[2-4 sentences explaining what they did]
- **Core Technique:** [Main approach]
- **Key Innovation:** [What makes it novel]
- **Fusion (if hybrid):** [How they combined techniques, e.g., "Attention (α) × RL reward (β)"]

### 📈 Key Results
[1-2 sentences on achievements, metrics, improvements over baselines]

### 💡 Relevance to My Work
| Question | Answer |
|----------|--------|
| Relevant? | ✅ Yes / ❌ No / 🔶 Partial |
| Why relevant? | [Reason] |
| Why irrelevant? | [Reason, if applicable] |
| ScalaTion applicable? | ✅ / ❌ / 🔶 |
| SUMO applicable? | ✅ / ❌ / 🔶 |
| Python notebook? | ✅ / ❌ / 🔶 |

### 🚀 Extension Opportunities
- [ ] [How this work could be extended]
- [ ] [What gap you could fill]
- [ ] [What you could combine it with]

### ⚠️ Limitations
- [Limitation 1]
- [Limitation 2]

### ⚡ Quick Take
[One sentence: Should you read fully? Skim? Implement? Skip?]
```

---

### 2. Markdown File (Detailed Summary)

Save to: `C:\Simulation\traffic-twin\papers\summaries\[paper-name].md`

Include everything from chat reply PLUS:

```markdown
# Paper Summary: [Title]

## Metadata
| Field | Value |
|-------|-------|
| Title | [Full title] |
| Authors | [All authors] |
| Venue | [Conference/Journal] |
| Year | [YYYY] |
| PDF Location | `[path]` |
| Read Date | [YYYY-MM-DD] |

## Abstract
> [Verbatim or close paraphrase of abstract]

## Problem Statement
[What problem does this paper solve? Why does it matter?]

## Paper Type & Context
[Detailed classification - put the paper in context of the field]

Examples of good contextualization:
- "This is a spatial-temporal GNN for traffic forecasting, following the STGCN lineage"
- "This is a transformer-based approach, applying the attention mechanism from Vaswani et al. to vehicle trajectories"
- "This is a model-based RL paper for adaptive signal control"
- "This is a classical calibration paper using SPSA optimization"
- "This is a hybrid fusing graph attention with reinforcement learning via [specific method]"

## Key Contributions
1. [Contribution 1]
2. [Contribution 2]
3. [Contribution 3]

## Dataset Deep Dive

| Dataset | Domain | Size | Public? | Familiarity |
|---------|--------|------|---------|-------------|
| [Name] | [Type] | [N samples/nodes] | ✅/❌ | ⭐ Standard / 🆕 New |

**Why This Dataset:** [Their justification]

**Dataset Context:**
- [Is this a benchmark everyone uses?]
- [Is this novel/proprietary?]
- [How does it compare to PeMS data you use?]

**Availability:** [Link or how to obtain]

## Methodology

### Approach Overview
[Detailed explanation of their method]

### Architecture / Model
[Describe the model structure]

### Key Equations (if applicable)
| Equation | Meaning | Novel? |
|----------|---------|--------|
| [Equation 1] | [What it computes] | ✅ Novel / ❌ Standard |

### Fusion Strategy (if hybrid)
- **Component A:** [First technique]
- **Component B:** [Second technique]  
- **How Combined:** [Weighted sum? Gating? Sequential? etc.]

### Training / Optimization
[How they trained the model, loss function, optimizer]

## Experiments & Results

| Experiment | Dataset | Metric | Their Result | Baseline |
|------------|---------|--------|--------------|----------|
| [Exp 1] | [Data] | [RMSE/MAPE/etc] | [Value] | [Compare] |

## Strengths
- [Strength 1]
- [Strength 2]

## Weaknesses / Limitations
- [Weakness 1]
- [Weakness 2]

## Relevance to My Work

### Similarities to Your Research
- [How is this similar to ScalaTion/your simulation work?]

### Differences from Your Research
- [How does your approach differ?]

### Why Relevant (if applicable)
- [Specific reasons this matters for your work]

### Why Irrelevant (if applicable)
- [Specific reasons this doesn't apply]

### Implementation Feasibility
| Platform | Feasible? | Effort | Notes |
|----------|-----------|--------|-------|
| ScalaTion | ✅/❌/🔶 | Low/Med/High | [Notes] |
| SUMO + TraCI | ✅/❌/🔶 | Low/Med/High | [Notes] |
| Python notebook | ✅/❌/🔶 | Low/Med/High | [Notes] |

## Extension Opportunities

### How to Extend This Work
1. [Extension idea 1]
2. [Extension idea 2]

### Gaps You Could Fill
- [Gap 1]
- [Gap 2]

### Potential Combinations
- [Could combine with X from your work]
- [Could apply to Y problem]

## Key Citations to Follow
- [Paper 1] — [Why worth reading]
- [Paper 2] — [Why worth reading]

## Personal Notes
[Any thoughts, questions, or ideas sparked by this paper]

---
*Summary generated: [DATE]*
```

---

## Critical Instructions for Agent

1. **Always contextualize the paper type** - Don't assume ML/GNN. Could be classical, RL, optimization, etc.

2. **Dataset is critical** - Always identify:
    - What dataset(s) they used
    - Why they chose it
    - Whether it's a standard benchmark or new
    - How it compares to user's PeMS data

3. **Be explicit about relevance** - State clearly:
    - "This is relevant because [X]"
    - "This is irrelevant because [Y]"
    - Don't be vague

4. **Identify extension opportunities** - Always think:
    - How could this be extended?
    - What's missing that user could add?
    - Could this combine with user's existing work?

5. **Feasibility assessment** - For each platform (ScalaTion, SUMO, Python), assess if implementation is feasible

6. **Create the output directory if it doesn't exist**

