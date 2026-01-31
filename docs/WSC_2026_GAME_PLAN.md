# Agentic Simulation Calibration Project

## One System → Two Papers

---

## The Two Perspectives

| Aspect | SIGDIAL | WSC |
|--------|---------|-----|
| **Angle** | Language & Tooling | Method & Digital Twin |
| **Research Question** | How does agent reason/communicate when calibrating? | Does agent beat human calibration? |
| **Focus** | Dialogue traces, tool-use patterns, reasoning chains | Accuracy metrics, convergence, streaming potential |
| **Key Metrics** | Dialogue coherence, tool invocation efficiency | R², RMSE, calibration time, parameter stability |
| **Audience** | NLP/Dialogue researchers | Simulation practitioners |
| **Contribution** | Dialogue act taxonomy, reasoning analysis | Autonomous calibration methodology |

---

## Topics to Cover

### SIGDIAL Paper Topics
- Tool-augmented dialogue systems
- Dialogue act taxonomy (QUERY, ADJUST, EVALUATE, REFLECT)
- Reasoning chain analysis in calibration context
- Memory and reflection in agentic systems
- Functional success vs conversational fluency
- Tool-use patterns and failure modes

### WSC Paper Topics
- Autonomous calibration methodology
- Agent vs human calibration comparison
- Real-time/streaming calibration feasibility
- Digital twin implications for traffic simulation
- Convergence analysis and parameter sensitivity
- Validation against NGSIM US-101 data

---

## Things to Learn

### Core Concepts
- [ ] ReAct pattern (Reasoning + Acting)
- [ ] Chain-of-Thought prompting
- [ ] Tool-use in LLMs (function calling)
- [ ] Digital twin fundamentals
- [ ] Simulation calibration methods (manual vs automated)

---

## Key Papers to Read

### Foundational Agentic AI Papers

| Paper | Year | Relevance | Code Available |
|-------|------|-----------|----------------|
| ReAct: Synergizing Reasoning and Acting in LLMs (Yao et al.) | 2023 | Core pattern for your agent | ✅ [github.com/ysymyth/ReAct](https://github.com/ysymyth/ReAct) |
| Toolformer: Language Models Can Teach Themselves to Use Tools (Schick et al.) | 2023 | Tool-use foundations | ❌ |
| HuggingGPT: Solving AI Tasks with ChatGPT and its Friends (Shen et al.) | 2023 | Multi-tool orchestration | ✅ [github.com/microsoft/JARVIS](https://github.com/microsoft/JARVIS) |
| Voyager: An Open-Ended Embodied Agent (Wang et al.) | 2023 | Skill library + memory | ✅ [github.com/MineDojo/Voyager](https://github.com/MineDojo/Voyager) |
| AutoGPT | 2023 | Full autonomous agent | ✅ [github.com/Significant-Gravitas/AutoGPT](https://github.com/Significant-Gravitas/AutoGPT) |

### Tool-Augmented Dialogue Papers (SIGDIAL-relevant)

| Paper | Year | Relevance | Code |
|-------|------|-----------|------|
| ToolLLM: Facilitating LLMs to Master 16000+ APIs (Qin et al.) | 2023 | Tool-use benchmark | ✅ [github.com/OpenBMB/ToolBench](https://github.com/OpenBMB/ToolBench) |
| API-Bank: A Benchmark for Tool-Augmented LLMs (Li et al.) | 2023 | Evaluation framework | ✅ [github.com/AlibabaResearch/DAMO-ConvAI/tree/main/api-bank](https://github.com/AlibabaResearch/DAMO-ConvAI/tree/main/api-bank) |
| TaskMatrix.AI (Liang et al.) | 2023 | Visual + API tools | ✅ [github.com/microsoft/TaskMatrix](https://github.com/microsoft/TaskMatrix) |

### Simulation Calibration Papers (WSC-relevant)

| Paper | Year | Relevance |
|-------|------|-----------|
| Automatic Calibration of Microscopic Traffic Simulation (Hollander & Liu) | 2008 | Classical approach |
| Digital Twin for Traffic Simulation (various) | 2020+ | Digital twin framing |
| LLM-based Scientific Discovery Agents | 2024 | Emerging area |

---

## Tools Needed

### Option A: Minimal (Recommended for Paper)

```
pip install openai        # Or: pip install anthropic
pip install pandas
pip install pyyaml        # Config handling
```

### Option B: With LangChain (More Features)

```
pip install langchain langchain-openai langgraph
pip install pandas pyyaml
```

### Option C: With LlamaIndex (Alternative)

```
pip install llama-index llama-index-llms-openai
pip install pandas pyyaml
```

---

## Framework Comparison

| Framework | Pros | Cons | Best For |
|-----------|------|------|----------|
| **Raw OpenAI API** | Simple, full control, easy to explain in paper | More manual code | ✅ Recommended |
| **LangChain** | Rich tooling, popular, good docs | Heavy, complex, changes often | If you need chains |
| **LangGraph** | Better for stateful agents | Steeper learning curve | Complex multi-step |
| **AutoGen (Microsoft)** | Multi-agent focus | Overkill for single agent | Multi-agent systems |

**Recommendation:** Start with raw OpenAI/Claude API. Easier to explain in paper, fewer dependencies.

---

## Your Custom Tools to Build

```
tools/
├── run_simulation.py      # Calls Scala sim via CLI, returns metrics
├── read_metrics.py        # Parses fitness.txt, arrival_error_timeseries.csv
├── adjust_parameters.py   # Modifies simulation config
├── get_current_state.py   # Returns current parameter values
└── compare_results.py     # Computes R², RMSE against observed
```

### Tool Definitions for LLM

```python
TOOLS = [
    {
        "name": "run_simulation",
        "description": "Execute traffic simulation with current parameters. Returns flow metrics.",
        "parameters": {"duration_minutes": "int"}
    },
    {
        "name": "adjust_parameter",
        "description": "Modify a simulation parameter.",
        "parameters": {"name": "string", "value": "float"}
    },
    {
        "name": "get_metrics",
        "description": "Get current R², RMSE, MAE against observed data.",
        "parameters": {}
    },
    {
        "name": "get_parameters",
        "description": "Get current simulation parameter values.",
        "parameters": {}
    }
]
```

---

## Minimal Architecture

```
┌─────────────────────────────────────┐
│       LLM (GPT-4 / Claude)          │
│  - Receives state + tool results    │
│  - Reasons about next action        │
│  - Outputs tool call or conclusion  │
└─────────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────┐
│       Agent Loop (Python)           │
│  - Memory: list of (action, result) │
│  - Reflection: analyze progress     │
│  - Planning: decide next step       │
│  - Termination: goal reached?       │
└─────────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────┐
│            Tools                    │
│  run_simulation → subprocess call   │
│  read_metrics   → parse CSV         │
│  adjust_params  → modify YAML       │
└─────────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────┐
│     Scala Traffic Simulation        │
│   (IDM, Krause, Erlang, etc.)       │
└─────────────────────────────────────┘
```

---

## Code Repositories to Study

### 1. ReAct Pattern Implementation
**Repo:** [github.com/ysymyth/ReAct](https://github.com/ysymyth/ReAct)
**Use:** Copy the reasoning+acting loop pattern

### 2. ToolBench (Tool-Use Benchmark)
**Repo:** [github.com/OpenBMB/ToolBench](https://github.com/OpenBMB/ToolBench)
**Use:** See how tools are defined and called

### 3. LangChain Tool Example
**Repo:** [github.com/langchain-ai/langchain](https://github.com/langchain-ai/langchain)
**File:** `libs/langchain/langchain/agents/`
**Use:** Agent loop structure (if you choose LangChain)

### 4. Voyager (Minecraft Agent)
**Repo:** [github.com/MineDojo/Voyager](https://github.com/MineDojo/Voyager)
**Use:** Memory + skill library pattern (overkill but instructive)

### 5. OpenAI Function Calling Examples
**Docs:** [platform.openai.com/docs/guides/function-calling](https://platform.openai.com/docs/guides/function-calling)
**Use:** Official pattern for tool-use

---

## Strategic Game Plan

### Phase 1: Foundation (Jan 13 - Jan 26) — 2 weeks

| Week | Tasks |
|------|-------|
| **Week 1** | Finish ANNSIM. Read ReAct paper. Get OpenAI/Claude API key. |
| **Week 2** | Build Python wrapper for Scala simulation. Test CLI invocation. |

**Deliverable:** Can call simulation from Python, get metrics back.

### Phase 2: Agent Core (Jan 27 - Feb 9) — 2 weeks

| Week | Tasks |
|------|-------|
| **Week 3** | Implement 4 tools (run\_simulation, read\_metrics, adjust\_params, get\_state). |
| **Week 4** | Build agent loop with memory. Test on simple calibration task. |

**Deliverable:** Agent can run 5-10 calibration iterations autonomously.

### Phase 3: Experiments (Feb 10 - Feb 23) — 2 weeks

| Week | Tasks |
|------|-------|
| **Week 5** | Run full calibration experiments. Collect dialogue traces. |
| **Week 6** | Compare agent vs manual calibration. Compute metrics. |

**Deliverable:** Data for both papers (dialogue traces + accuracy metrics).

### Phase 4: Writing (Feb 24 - Mar 26) — 4 weeks

| Week | Tasks |
|------|-------|
| **Week 7** | Write WSC paper (method focus). |
| **Week 8** | SIGDIAL abstract due (Mar 6). Submit abstract. |
| **Week 9-10** | Write SIGDIAL short paper (dialogue focus). Due Mar 26. |

---

## Tactical Checklist

### Before You Start Coding
- [ ] Get OpenAI API key (or Claude/Anthropic)
- [ ] Read ReAct paper (30 min)
- [ ] Clone ReAct repo, run their example
- [ ] Decide: Raw API vs LangChain (recommend: Raw API)

### Week 1-2 Deliverables
- [ ] Python script that calls your Scala simulation
- [ ] Parser for `arrival_error_timeseries.csv` (you have this file)
- [ ] Function to compute R² from observed vs simulated

### Week 3-4 Deliverables
- [ ] 4 tool functions working
- [ ] Agent loop that: prompt → parse → execute → repeat
- [ ] Memory list storing (action, result, reflection)

### Week 5-6 Deliverables
- [ ] 10+ calibration runs with dialogue traces saved
- [ ] Comparison table: Agent R² vs Manual R²
- [ ] Convergence plot (R² over iterations)

---

## File Structure for Project

```
agentic-calibration/
├── agent/
│   ├── main.py              # Agent loop
│   ├── memory.py            # Stores past attempts
│   ├── prompts.py           # System prompts, tool descriptions
│   └── tools.py             # Tool definitions + implementations
├── simulation/
│   └── run_scala.py         # Subprocess wrapper for Scala sim
├── analysis/
│   ├── compute_metrics.py   # R², RMSE, MAE
│   └── plot_convergence.py  # Visualization
├── logs/
│   └── dialogue_traces/     # JSON logs of each run
├── config/
│   └── simulation.yaml      # Parameters to calibrate
└── README.md
```

---

## Key Decisions to Make

| Decision | Options | Recommendation |
|----------|---------|----------------|
| LLM Provider | OpenAI GPT-4 / Anthropic Claude | Either works. GPT-4 has better function calling. |
| Framework | Raw API / LangChain / LangGraph | Raw API for simplicity |
| Agent Pattern | ReAct / Plan-Execute / Tree-of-Thought | ReAct (proven, simple) |
| Memory | Simple list / Vector DB | Simple list (sufficient for paper) |

---

## Success Criteria

### For SIGDIAL Acceptance
- Clear dialogue traces showing reasoning
- Analysis of dialogue acts and tool-use patterns
- Novel application domain (traffic simulation)

### For WSC Acceptance
- Agent matches or beats manual calibration
- Clear methodology description
- Implications for digital twin / streaming

---

## Risk Mitigation

| Risk | Mitigation |
|------|------------|
| Agent doesn't converge | Pre-define parameter bounds, add fallback heuristics |
| Too slow | Limit iterations, use smaller simulation runs |
| LLM costs too high | Use GPT-3.5 for testing, GPT-4 for final runs |
| Not enough novelty | Emphasize traffic domain application (novel context) |