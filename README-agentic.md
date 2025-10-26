# Runix: Agentic Framework Architecture

Runix is a comprehensive agentic coding framework that combines the safety and composability of effect systems with the intelligence and flexibility of LLM-powered agents. It solves the fundamental scaling problems that plague current agent frameworks through type-safe composition, effect-based capability control, and intelligent resource management.

## Architecture Overview

Runix provides a production-ready agentic framework built on these core principles:

- **Type-safe agent composition** with compile-time guarantees
- **Effect-based capability control** for secure agent execution  
- **Intelligent resource management** with cost optimization
- **Hybrid deterministic-agentic workflows** that use the right tool for each job
- **Persistent learning** that improves agent performance over time

## System Components

### Core Foundation
- **Polysemy-based effect system** with `FileSystem`, `HTTP`, `LLM`, `CompileTask` effects
- **Type-safe LLM integration** with `LLMInput`/`LLMOutput` abstractions
- **Task composition system** supporting both deterministic and agentic workflows
- **Nix-based build system** for reproducible environments
- **Safe Haskell enforcement** for security guarantees

### Agentic Extensions  
- **Agent loop primitives** for LLM reasoning and tool execution
- **Smart LLM routing** with multi-provider support and optimization
- **Stateful agent sessions** with context preservation and resumption
- **Multi-agent coordination** patterns for complex workflows
- **Persistent memory system** for cross-session learning and improvement

## Key Design Principles

### 1. Principled Division of Labor
Unlike frameworks that use LLMs for everything, Runix employs **hybrid execution** where each component handles what it does best:

```haskell
-- Complex analysis: deterministic, fast, testable
analyzeCodeStructure :: Codebase -> StructuralAnalysis

-- Creative coordination: LLM-powered, flexible  
coordinateRefactoring :: Members '[LLM, ToolExecution] r => 
  StructuralAnalysis -> Sem r RefactoringPlan

-- Validation: deterministic, reliable
validateRefactoring :: RefactoringPlan -> ValidationResult
```

This division provides better quality, lower costs, predictable performance, and maintainable results.

### 2. Effect-Based Capability Control
The effect system ensures agents get exactly the capabilities they need through compile-time constraint verification:

```haskell
-- Analysis agent: Can read, cannot modify
analysisAgent :: Members '[FileSystem, LLM] r => 
  Codebase -> Sem r AnalysisReport

-- Code generation agent: Can write to specific areas
generatorAgent :: Members '[LimitedFileSystem, LLM, CompilerCheck] r =>
  Spec -> Sem r GeneratedCode

-- Full workflow agent: Broader capabilities with validation
coordinatorAgent :: Members '[FileSystem, LLM, TestRunner, AgentCoordination] r =>
  Problem -> Sem r Solution
```

This approach provides security through least privilege, simplified agent reasoning, and controlled blast radius.

### 3. Semantic LLM Selection
Higher-kinded effects enable both semantic intent and explicit model choice, with automatic routing optimization:

```haskell
-- Semantic usage (automatic routing to appropriate models)
transcriptProcessor :: Members '[LLM 'Bulk] r => RawTranscript -> Sem r CleanTranscript
codeReviewer :: Members '[LLM 'Coding] r => CodeSubmission -> Sem r Review
strategicPlanner :: Members '[LLM 'Reasoning] r => Problem -> Sem r Strategy

-- Explicit model choice when needed
criticalAnalysis :: Members '[LLM 'Claude35Sonnet] r => CriticalProblem -> Sem r Analysis
```

The system automatically optimizes for cost, performance, provider availability, and maintains semantic clarity.

### 4. Evolutionary Agent Development
The framework supports evolution from dynamic exploration to compiled optimization:

**Phase 1: Dynamic Exploration**
```haskell
-- General-purpose agent for rapid iteration
dynamicAgent :: Members '[LLM, ToolExecution] r => 
  Text -> Context -> Sem r (Result, AgentTrace)
```

**Phase 2: Pattern Recognition**  
```haskell
-- Extract successful patterns from execution traces
generateConcreteAgent :: [AgentTrace] -> AgentDescription -> Sem r HaskellCode
```

**Phase 3: Compiled Quality**
```haskell
-- Type-safe, optimized implementation with quality guarantees
compiledAgent :: Members '[SpecificEffects] r => TypedInput -> Sem r TypedOutput
```

This evolutionary approach enables rapid prototyping with eventual production quality, providing the best of both worlds.

## Implementation Architecture

### Agent Loop Framework
The core agent reasoning system provides LLM-powered decision making with tool execution:

```haskell
data ToolExecution m a where
  ExecuteTool :: ToolName -> ToolArgs -> ToolExecution m ToolResult
  ListAvailableTools :: ToolExecution m [ToolName]

-- Agent reasoning loop with conversation state management
agentLoop :: Members '[LLM, ToolExecution] r => 
  SystemPrompt -> Goal -> [Message] -> Sem r Result
```

Agents use this framework to reason about problems, select appropriate tools, execute actions, and iterate based on results.

### Smart LLM Routing
The LLM routing system automatically selects optimal models based on task requirements and resource constraints:

```haskell
-- Higher-kinded LLM effect with semantic selectors
data LLM (selector :: k) (m :: Type -> Type) a where
  AskLLM :: Prompt -> LLM selector m Response
  QueryLLM :: Instructions -> Context -> LLM selector m Response

-- Automatic routing based on selector type and available providers
runSmartLLM :: LLMPoolConfig -> Sem (LLM selector : r) a -> Sem r a
```

The system manages multiple providers (Anthropic, OpenAI, local Ollama, OpenRouter), automatically selecting the most cost-effective and capable model for each task while respecting rate limits and budget constraints.

### Agent Session Management
Stateful agent sessions enable context preservation and intelligent resumption across multiple interactions:

```haskell
data AgentSession m a where
  StartSession :: AgentSpec -> InitialContext -> AgentSession m SessionId
  RunSession :: SessionId -> Goal -> AgentSession m SessionResult
  ResumeSession :: SessionId -> AdditionalContext -> Goal -> AgentSession m SessionResult
  GetSessionHistory :: SessionId -> AgentSession m ConversationHistory
```

Sessions maintain conversation history, track attempted approaches, and enable agents to learn from previous interactions, avoiding repetitive failures and building on successful strategies.

### Resource Budget Management
Budget constraints are applied through effect interpretation, similar to filesystem permissions:

```haskell
-- Budget constraints applied like filesystem permissions
withBudget :: Amount -> ResourceType -> Sem (LLM : r) a -> Sem (LLM : r) a
withSoftLimit :: Percentage -> Sem (LLM : r) a -> Sem (LLM : r) a

-- Automatic degradation when approaching limits
budgetAwareLLM :: Members '[ResourceBudget, LLM] r => 
  AgentId -> Prompt -> Sem r Response
```

The system provides soft limits (warning agents to conclude quickly) and hard limits (immediate termination), enabling graceful degradation while preventing budget overruns.

### Persistent Memory System
Cross-session memory enables agents to learn and improve over time:

```haskell
data PersistentMemory m a where
  StoreMemory :: MemoryType -> Key -> Value -> Timestamp -> PersistentMemory m ()
  QueryMemory :: MemoryType -> Query -> PersistentMemory m [MemoryEntry]
  StoreLearning :: LearningType -> Pattern -> Outcome -> PersistentMemory m ()

-- Agents automatically benefit from cross-session learning
memoryAwareAgent :: Members '[PersistentMemory, LLM, ToolExecution] r =>
  Problem -> Sem r Solution
```

The memory system tracks successful patterns, tool effectiveness, problem solutions, and user preferences, allowing agents to make better decisions based on historical experience.

### Workflow Automation Hooks
The hook system provides automatic workflow enhancement without manual intervention:

```haskell
data WorkflowHooks m a where
  RegisterPreHook :: HookPoint -> HookAction -> WorkflowHooks m HookId
  RegisterPostHook :: HookPoint -> HookAction -> WorkflowHooks m HookId
  TriggerHooks :: HookPoint -> HookContext -> WorkflowHooks m [HookResult]

-- Automatic workflow enhancement without manual intervention
enhancedAgent :: Members '[WorkflowHooks, LLM, ToolExecution] r =>
  Problem -> Sem r Solution
```

Hooks automatically trigger at workflow points to restore context, apply optimizations, generate tests, and perform quality checks, enhancing agent workflows transparently.

## Usage Examples

### Simple Agent Task
```haskell
-- Basic agent that analyzes code and suggests improvements
codeAnalysisAgent :: Members '[LLM 'Reasoning, ToolExecution, FileSystem] r =>
  Codebase -> Sem r AnalysisReport
codeAnalysisAgent codebase = do
  -- Agent automatically uses appropriate reasoning model
  analysis <- askLLM @'Reasoning $ "Analyze this codebase for improvements: " <> show codebase
  
  -- Agent can execute tools to gather more information
  testCoverage <- executeTool "analyze_test_coverage" (toJSON codebase)
  complexity <- executeTool "calculate_complexity" (toJSON codebase)
  
  -- Combine results into structured report
  return $ AnalysisReport analysis testCoverage complexity
```

### Multi-Agent Coordination
```haskell
-- Coordinator agent that manages specialist subagents
debuggingCoordinator :: Members '[AgentSession, LLM 'Reasoning, ToolExecution] r =>
  BugReport -> Sem r DebuggingResult
debuggingCoordinator bugReport = do
  -- Start specialized debugging session
  debugSession <- startSession debuggerAgentSpec (BugContext bugReport)
  
  -- Let agent work on the problem
  firstAttempt <- runSession debugSession "Fix this null pointer exception"
  
  -- Test the fix
  testResult <- executeTool "run_tests" (toJSON firstAttempt.solution)
  
  case testResult of
    TestsPassed -> return $ SuccessfulFix firstAttempt.solution
    TestsFailed errors -> do
      -- Resume agent with failure context
      secondAttempt <- resumeSession debugSession 
        (TestFailureContext errors) 
        "Previous fix didn't work. Try a different approach."
      return $ ImprovedFix secondAttempt.solution
```

### Resource-Optimized Processing
```haskell
-- Bulk processing with automatic model optimization
transcriptProcessor :: Members '[LLM 'Bulk, ResourceBudget] r =>
  [RawTranscript] -> Sem r [ProcessedTranscript]
transcriptProcessor transcripts = 
  withBudget 1000 LLMTokens $  -- Set budget constraint
  withSoftLimit 80 $           -- Warn at 80% usage
  mapM processChunk transcripts
  where
    processChunk chunk = do
      -- Automatically uses fast, cheap model (e.g., local Ollama)
      cleaned <- askLLM @'Bulk $ "Fix grammar and spelling: " <> chunk
      return $ ProcessedTranscript cleaned
```

## Advantages Over Current Agent Frameworks

### Compared to Other Agent Frameworks

**vs Claude Flow**: 
- Compile-time type safety prevents runtime errors
- Effect-based isolation controls agent capabilities precisely  
- Built-in resource management vs ad-hoc budget limiting
- Functional composition patterns vs complex coordination logic

**vs LangChain/AutoGPT**:
- Predictable execution through effect constraints
- Hybrid deterministic-agentic execution vs all-LLM overhead
- Compile-time validation catches errors early
- Intelligent model routing optimizes costs automatically

**vs Traditional Programming**:
- Agent reasoning adapts to unexpected situations
- Natural language instructions reduce development friction
- Dynamic problem exploration vs predetermined control flow
- Conversational workflows enable better user interaction

## Key Benefits

### Technical Advantages
- **Type Safety**: All agent workflows are compile-time verified, preventing runtime errors
- **Resource Efficiency**: Intelligent model routing minimizes costs while maintaining quality
- **Predictable Performance**: Hybrid execution provides consistent, measurable outcomes
- **Secure Execution**: Effect-based capabilities prevent unauthorized access and actions

### Development Benefits
- **Rapid Prototyping**: Dynamic agents enable fast exploration and iteration
- **Production Quality**: Compiled agents provide industrial-strength reliability
- **Easy Composition**: Functional patterns make complex workflows manageable
- **Maintainable Code**: Type system and effect constraints enforce good architecture

### Operational Advantages
- **Cost Optimization**: Automatic model selection based on task requirements and budget
- **Quality Assurance**: Deterministic components handle complex logic reliably
- **Scalable Architecture**: Effect system enables fine-grained control at any scale
- **Learning System**: Persistent memory improves agent performance over time

## Research Directions

The Runix agentic framework opens several areas for future research and development:

### Economic Intelligence
- Dynamic cost optimization across multiple LLM providers
- Quality-cost tradeoff modeling and automated optimization
- Temporal resource allocation based on pricing patterns
- Bulk request optimization for volume discounts

### Agent Coordination
- Hierarchical multi-agent coordination patterns
- Cross-session learning and knowledge transfer
- Automatic workflow pattern recognition and optimization
- Failure analysis and recovery strategy development

### Effect System Innovation
- Advanced capability composition strategies
- Dynamic constraint satisfaction algorithms
- Fine-grained security boundary management
- Novel effect interpretation patterns for AI workloads

## Getting Started

### Installation
```bash
# Clone and build Runix with agentic extensions
git clone https://github.com/user/runix  
cd runix
nix build

# Verify installation
./result/bin/runix-cli --version
```

### Basic Usage
```bash
# Run a deterministic task
./result/bin/runix-cli hello

# Run an agentic task
./result/bin/runix-cli agent --goal "Analyze codebase for performance issues"

# Run with specific LLM configuration
./result/bin/runix-cli agent --config anthropic.json --goal "Debug null pointer exception"

# Run with resource constraints
./result/bin/runix-cli agent --budget 1000 --goal "Bulk process transcripts"
```

### Configuration
Create an LLM provider configuration:
```json
{
  "providers": [
    {
      "type": "anthropic",
      "apiKey": "your-key-here",
      "models": ["claude-3-5-sonnet", "claude-3-haiku"],
      "defaultFor": ["coding", "reasoning"]
    },
    {
      "type": "ollama", 
      "endpoint": "http://localhost:11434",
      "models": ["phi-3", "llama-3-8b"],
      "defaultFor": ["bulk"]
    }
  ],
  "routing": {
    "optimizeFor": "cost",
    "fallbackStrategy": "degradeGracefully"
  }
}
```

### Creating Custom Agents
```haskell
-- Define a custom agent task
myAnalysisAgent :: Members '[LLM 'Reasoning, FileSystem, ToolExecution] r =>
  ProjectPath -> Sem r AnalysisResult
myAnalysisAgent projectPath = do
  files <- listProjectFiles projectPath
  analysis <- analyzeProjectStructure files
  suggestions <- generateImprovements analysis
  return $ AnalysisResult analysis suggestions
```

## Architecture Summary

Runix's agentic framework solves fundamental challenges in AI-powered development through:

**Type-Safe Composition**: All agent interactions are verified at compile time, preventing runtime errors and ensuring reliable behavior.

**Effect-Based Security**: Agents receive precisely the capabilities they need through the effect system, ensuring secure execution with minimal privileges.

**Intelligent Resource Management**: Automatic model selection optimizes for cost, quality, and performance while respecting budget constraints and rate limits.

**Hybrid Execution Model**: Deterministic code handles complex logic while agents provide intelligent coordination, resulting in better quality and lower costs than pure-LLM approaches.

**Persistent Learning**: Cross-session memory enables agents to improve over time, learning from successful patterns and avoiding repeated failures.

**Seamless Scalability**: The effect system enables fine-grained control from simple single-agent tasks to complex multi-agent coordination workflows.

This architecture provides a production-ready foundation for AI-powered development tools that are both more capable and more reliable than current alternatives, while maintaining the safety and predictability expected in professional software development environments.