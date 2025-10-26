# How to Create Runix Tasks with AI Assistants

A practical guide based on real development experience, capturing key insights for humans working with AI to create type-safe, functional automation tasks.

## Core Philosophy

**The AI is your thinking partner, not your coding machine.** The best results come from collaborative problem-solving where you provide domain knowledge and architectural judgment while the AI handles syntax, pattern recognition, and implementation details.

## Key Success Patterns

### 1. Start with Your Problem, Not the Solution

**Do this:**
- Describe what you want to accomplish in your own words
- Provide concrete examples of inputs and desired outputs
- Explain why this matters to you personally

**Don't do this:**
- Jump straight to technical specifications
- Assume the AI knows your domain context
- Start with "I need a function that..."

**Why it works:** The AI excels at translating human problems into technical solutions, but struggles with reverse-engineering intent from technical specs.

### 2. Embrace the "Obvious" Questions

When the AI asks clarifying questions that seem obvious to you, resist the urge to brush them off. These questions often reveal important assumptions or edge cases you haven't considered.

**Example from our session:**
- **AI:** "What format are the transcripts in?"
- **Human reaction:** "Obviously text files, what else would they be?"
- **Better response:** "Text files from audio transcription - they might have timestamps and speaker identification"

This small clarification shaped the entire filtering approach.

### 3. Let Architecture Emerge, Don't Force It

**The best architectural decisions in our session emerged organically:**
- We didn't plan the `Wiki` effect - it emerged when we noticed parameter threading
- Module extraction happened when we felt the abstraction was ready
- Effect reinterpretation came up naturally when discussing safety

**Trust the process:** Follow the workflow steps, but stay open to architectural insights that emerge during implementation.

### 4. Use Your "Code Smell" Instincts

Even if you're not a Haskell expert, you can recognize when something feels wrong:
- "This parameter is being passed everywhere"
- "These functions all do similar things"
- "This feels more complicated than it should be"

**Share these feelings with the AI** - they're often signals that refactoring or abstraction is needed.

### 5. Correct Early and Often

Don't wait for the AI to get everything perfect before providing feedback. Small course corrections are much more effective than major overhauls.

**Good correction pattern:**
- **AI:** "I'll create a complex parsing system for the transcript"
- **Human:** "Actually, let's just let the LLM handle the semantic parsing"
- **Result:** Much simpler, more robust solution

### 6. Leverage Your Domain Knowledge

The AI doesn't know about:
- Logseq filename conventions
- Your specific D&D campaign needs  
- Why certain abstractions matter in your context

**Be generous with context** - share the "why" behind your preferences.

### 7. Distinguish Between "Nice to Have" and "Essential"

**Essential insights from our session:**
- Type safety through newtype wrappers
- Keeping code compilable with `undefined` stubs
- Effect-based architecture

**Nice to have:**
- Perfect error messages
- Optimal performance
- Complete feature coverage

Focus on getting the essential patterns right first.

## Collaboration Techniques

### The "Yes, And..." Approach

When the AI suggests something you didn't think of, explore it before rejecting:

**Example:**
- **AI:** "Should we extract this to a separate module?"
- **Instead of:** "No, let's keep it simple"
- **Try:** "What would that look like? What are the benefits?"

This led to our Wiki module extraction, which was a major improvement.

### Share Your Uncertainty

**When you said:** "I'm worried I might be overcomplicating things with effects..."

**This was valuable because:**
- It gave the AI permission to explore simpler alternatives
- It revealed your values (simplicity over abstraction)
- It led to a better discussion about trade-offs

### Ask for Architectural Perspective

**Useful questions:**
- "Does this feel like the right level of abstraction?"
- "What patterns do you see emerging here?"
- "If this were your codebase, what would concern you?"

## What to Expect from the AI

### AI Strengths
- **Pattern recognition:** Spotting repeated code structures
- **Translation:** Converting between languages/paradigms  
- **Syntax handling:** Getting Haskell details right
- **Research:** Looking up domain-specific information

### AI Limitations
- **Domain expertise:** Doesn't understand your specific needs
- **Long-term vision:** Can't see the bigger architectural picture
- **Value judgments:** Doesn't know what trade-offs matter to you
- **Context limits:** May forget earlier decisions in long sessions

### Work with These Limitations

**Instead of expecting the AI to be an expert in your domain:**
- Provide context liberally
- Explain your reasoning behind preferences
- Be explicit about constraints and requirements

## Red Flags and Course Corrections

### When to Push Back

**If the AI suggests:**
- Overly complex abstractions early in development
- Solutions that require deep framework knowledge you don't have
- Patterns that feel unfamiliar to your domain

**Ask:** "Is there a simpler way?" or "What would the minimal version look like?"

### When to Trust the AI

**If the AI suggests:**
- Type safety improvements (like newtype wrappers)
- Refactoring to reduce code duplication
- Following established functional programming patterns

**These are usually worth exploring** - the AI is good at recognizing beneficial patterns.

## Practical Session Management

### Keep Context Alive

- Reference earlier decisions: "Remember when we chose to use newtypes for type safety..."
- Summarize progress periodically: "So far we have X, Y, and Z working..."
- Document insights as they emerge

### Use Iterative Validation

After each major step:
1. **Check:** Does this still solve my original problem?
2. **Test:** Can I explain what this code does in plain English?
3. **Evaluate:** Does this feel simpler or more complex than before?

### Plan for Handoff

Assume you'll need to maintain this code without the AI:
- **Understand the key abstractions** (even if not every detail)
- **Document the why, not just the what**
- **Ensure you can modify it for future needs**

## Final Insights

### The Best AI Sessions Feel Like Pair Programming

You're not delegating to the AI - you're thinking together. The AI handles syntax and patterns while you provide vision and judgment.

### Architecture Emerges from Solving Real Problems

Don't try to design the perfect system upfront. Build something that works, then refactor when you feel the friction. Good abstractions reveal themselves through use.

### Trust the Process, Guide the Direction

The incremental workflow (requirements → pseudocode → types → implementation) works well, but you need to steer it toward solutions that match your values and constraints.

### Your Domain Knowledge is Irreplaceable

The AI can write code, but only you know whether that code solves the right problem in the right way for your specific context. That judgment is the most valuable part of the collaboration.

---

*This guide emerged from developing a D&D wiki generator task, but the patterns apply broadly to functional programming projects with AI assistance.*