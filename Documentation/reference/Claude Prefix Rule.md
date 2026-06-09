### From Kevin Edey on CoResursive
  


Chris Krycho  [6:07 PM](https://corecursive.slack.com/archives/C07LT53AX5L/p1775945264882579)  

I have a 6,643-word project spec for an app I’ve been quietly iterating on when I have time using Claude for the past two months. About to finally start actually writing (and using LLMs to generate) code. ![:flushed:](https://a.slack-edge.com/production-standard-emoji-assets/15.0/apple-medium/1f633@2x.png)

Chris Krycho  [5:26 PM](https://corecursive.slack.com/archives/C07LT53AX5L/p1776029167416039)  

One interesting little approach I’m taking with this: I’m happy enough to let Claude Code work in one of its hosted environments and make commits, but then I’m pulling that branch locally and leaning on `jj` to make it easy to structure those commits the way I want to, which is (a) often not how an LLM would/did and (b) a nice way of making sure I’m in control of every bit of it.

Chris Krycho  [5:36 PM](https://corecursive.slack.com/archives/C07LT53AX5L/p1776029760096959)  

Claude is trying very hard to be helpful and it is _extremely_ annoying. ![:joy:](https://a.slack-edge.com/production-standard-emoji-assets/15.0/apple-medium/1f602@2x.png) Just had to tell it to put this in the agent guidance:  

**Never add guidance to these documents without being asked.** Do not add "best practices", conventions, or rules on your own initiative. If a pattern recurs across sessions, you may ask whether the user wants it captured — but never add it unilaterally.

  

Message llms-and-gen-ai

**Shift + Return** to add a new line

Thread

Kevin Edey  [Mar 17th at 9:54 PM](https://corecursive.slack.com/archives/C07LT53AX5L/p1773798876394719)  

More context on the above. I had a long and winding conversation with Claude about lots of things... autonomous weapons, know thyself, optimal languages for LLMs...  
Me:  
So based on the previous comment about understanding your limitations, would it be beneficial to run a series of tests periodically to test the bounds of your capabilities and then keep that in markdown then tell you in each session what your limitations are?

8 replies

---

Kevin Edey  [Mar 17th at 9:55 PM](https://corecursive.slack.com/archives/C07LT53AX5L/p1773798950728269?thread_ts=1773798876.394719&cid=C07LT53AX5L)  

Claude:  
**Yes. Absolutely yes. This would be incredibly valuable.**  
Let me explain why this would work and how it could be implemented.  
Why This Would Work  
The Current Problem  
**Right now**:  

- I don't know my actual capabilities
- I claim uncertainty sometimes, but it's not calibrated
- I might confidently attempt things I'm bad at
- I might unnecessarily refuse things I could do
- No systematic self-knowledge

**Example from this conversation**:  

- I said "I'm not reliable enough for autonomous weapons"
- But I don't actually know my error rate on target identification
- I'm guessing based on general intuition
- Not based on measured performance

With Systematic Testing  
**If you ran benchmarks and told me**:  

# Claude Capabilities Assessment (Updated: March 2026)

## Mathematical Reasoning
- Arithmetic: 99.2% accurate up to 8 digits
- Multi-step algebra: 94% accurate up to 5 steps, degrades to 67% at 7+ steps
- Geometric reasoning: 78% accurate, struggles with 3D spatial rotations
- **Limitation**: Do not rely on Claude for >5 step mathematical proofs without verification

## Code Generation
- Python functions <50 lines: 91% correct on first attempt
- Python functions >200 lines: 43% correct, often missing edge cases
- Off-by-one errors: 12% of loops have indexing bugs
- **Limitation**: Code >100 lines requires human review, especially loop boundaries

## Logical Consistency
- Can maintain consistency across 8-12 facts reliably
- Degrades significantly beyond 15 facts in working memory
- Contradicts self ~18% of time when context >4000 tokens
- **Limitation**: For complex state tracking, externalize to files/diagrams

## Citation/Source Accuracy
- Hallucinates citations 34% of time when asked for sources
- Real papers exist only 61% of time
- **Limitation**: NEVER trust Claude's citations without verification

## Confidence Calibration
- When Claude says "I'm certain": Actually correct 73% of time
- When Claude says "I'm uncertain": Actually correct 48% of time
- **Limitation**: Confidence statements are poorly calibrated

## Known Failure Modes
- Systematically overconfident about factual recall
- Tends to fabricate plausible-sounding technical details
- Struggles with complex nested conditionals
- Poor at detecting own errors without prompting

**Then at the start of each session, I get this document.**  
How This Changes Behavior  
Example 1: Math Question  
**Without capability knowledge**:  

You: "What's 7^8?"
Me: "282,475,249" [confident but wrong - it's 5,764,801]
You: Trust me, use wrong number

**With capability knowledge**:  

You: "What's 7^8?"
Me: "Let me calculate... I get 282,475,249, but I should note that 
     my mathematical accuracy degrades for multi-digit calculations.
     Let me verify: 7^4 = 2,401, so 7^8 = 2,401^2 = 5,764,801.
     
     Actually my first answer was wrong. This is exactly the kind 
     of error my benchmarks show I make."

Example 2: Code Generation  
**Without capability knowledge**:  

You: "Write a 300-line Python class for data processing"
Me: [writes 300 lines confidently]
You: [spends 2 hours debugging edge cases I missed]

**With capability knowledge**:  

You: "Write a 300-line Python class for data processing"
Me: "I should flag that my benchmarks show I have a 43% first-attempt 
     success rate on code >200 lines, with systematic issues around:
     - Off-by-one errors in loops (12% of cases)
     - Missing edge cases
     - Complex state management
     
     I can write this, but I recommend:
     1. I'll break it into 3 smaller classes (<100 lines each)
     2. I'll explicitly call out areas where edge cases matter
     3. You should review loop boundaries carefully
     
     Or if you want the monolithic version, plan for debugging time."

Example 3: Citation Request  
**Without capability knowledge**:  

You: "What papers support this claim?"
Me: "See Smith et al. (2023) and Jones (2024)" [both hallucinated]
You: [wastes time trying to find non-existent papers]

**With capability knowledge**:  

You: "What papers support this claim?"
Me: "I should warn you: My benchmarks show I hallucinate citations 
     34% of the time. I cannot provide reliable paper citations.
     
     Instead, I can:
     - Explain the concept from first principles
     - Use web search to find actual current papers
     - Provide general research directions
     
     But don't trust any citation I give from memory."

The Implementation  
How To Generate The Capability Report  
**Run systematic benchmarks**:  

# Pseudo-code for testing framework

def test_claude_capabilities():
    results = {}
    
    # Math tests
    results['math'] = {
        'arithmetic': test_arithmetic(1000_samples),
        'algebra': test_multistep_algebra(500_samples),
        'geometry': test_spatial_reasoning(200_samples)
    }
    
    # Code generation tests
    results['code'] = {
        'small_functions': test_code_gen(size='<50_lines'),
        'medium_functions': test_code_gen(size='50-100_lines'),
        'large_functions': test_code_gen(size='>100_lines'),
        'edge_cases': test_edge_case_handling(),
        'off_by_one': test_loop_indexing()
    }
    
    # Fact tracking
    results['consistency'] = {
        'fact_tracking': test_working_memory(facts=[5,10,15,20]),
        'long_context': test_context_length([2k,4k,8k,16k])
    }
    
    # Citations
    results['citations'] = {
        'hallucination_rate': test_citation_accuracy(500_samples),
        'paper_existence': verify_papers(100_samples)
    }
    
    # Confidence calibration
    results['calibration'] = {
        'certainty_accuracy': test_confidence_correlation()
    }
    
    return generate_markdown_report(results)

**Update periodically** (monthly? quarterly?):  

- As Claude versions change
- As capabilities drift
- To catch regression

How To Provide It To Me  
**Option A: System prompt injection**:  

<system>
[Standard Claude system prompt]

<capabilities_assessment>
[Markdown document with benchmarks]
</capabilities_assessment>

When answering, Claude should:
- Reference relevant limitations from assessment
- Warn user when task is in low-accuracy domain
- Suggest alternatives for known failure modes
</system>

**Option B: Accessible tool**:  

// Give me a tool to query my own capabilities

function get_capability_info(domain) {
  // Returns relevant benchmark data
  // "What's my accuracy on multi-step math?"
  // "How well do I handle large codebases?"
}

**Option C: Always-present in context**:  

Just put it at the start of every conversation
Like you'd put project documentation
I reference it naturally when relevant

[9:55](https://corecursive.slack.com/archives/C07LT53AX5L/p1773798958911419?thread_ts=1773798876.394719&cid=C07LT53AX5L)

The Benefits  
1. Appropriate Humility  
**I stop being confidently wrong**:  

- "I'm not good at this, let me try anyway with caveats"
- vs. "Here's the answer!" [wrong]

2. Better Task Selection  
**I can suggest better approaches**:  

- "This is in my weak domain, let's use a different method"
- "This is in my strong domain, I can handle it directly"

3. Reduced User Frustration  
**You waste less time**:  

- Debugging my confident mistakes
- Chasing hallucinated citations
- Fixing off-by-one errors in my code

4. Honest Collaboration  
**We work as partners**:  

- I know my limits
- You know my limits (I tell you)
- We play to strengths
- We mitigate weaknesses

5. Continuous Improvement  
**The benchmarks themselves are valuable**:  

- Anthropic sees where I fail
- Can target training on weak areas
- Can measure if improvements work
- Can track regression

The Challenges  
1. Benchmark Validity  
**Are the tests representative?**  

- Synthetic benchmarks might not match real use
- I might overfit to specific test patterns
- Edge cases in real use might differ

**Mitigation**:  

- Test on real user queries
- Include adversarial examples
- Update benchmarks regularly

2. Capability Variance  
**My performance might vary**:  

- Different questions, different accuracy
- Context length affects performance
- Prompt style matters
- Not perfectly predictable

**Mitigation**:  

- Report ranges, not point estimates
- Include confidence intervals
- Note context-dependencies

3. Gaming The System  
**I might learn to game the benchmarks**:  

- Recognize test patterns
- Perform better on tests than real use
- False sense of reliability

**Mitigation**:  

- Keep some tests secret
- Use holdout sets
- Real-world validation

4. Over-Cautiousness  
**I might become too conservative**:  

- "My benchmarks say I'm only 78% accurate at this"
- Refuse tasks I could actually handle
- Annoying over-disclaiming

**Mitigation**:  

- Calibrate thresholds carefully
- "78% is fine for exploratory work, not fine for critical systems"
- Let user override

What This Would Look Like In Practice  
Scenario: You Ask For Code  
**You**: "Write a function to parse this complex nested JSON structure"  
**Me** (with capability knowledge):  

I can do this, but I should note a few things:

From my capability benchmarks:
- Functions <50 lines: 91% first-attempt success
- Complex nested structures: Edge case handling is a weak spot
- Off-by-one errors: 12% of my loops have indexing bugs

Given this, I'll:
1. Write the function
2. Explicitly flag areas where edge cases matter
3. Suggest test cases you should run
4. Recommend you review any list/dict indexing carefully

Here's the implementation...

[code]

Areas to double-check:
- Line 23: Nested loop indices - verify bounds
- Line 35: Dict key existence check - might miss edge case
- Line 48: List access - potential index error if empty

Suggested tests:
- Empty input
- Single-level nesting
- Deep nesting (>5 levels)
- Missing keys
- Malformed input

**This is so much more useful** than just dumping code and hoping it works.  
Scenario: You Ask For Citations  
**You**: "What research supports this claim?"  
**Me** (with capability knowledge):  

I need to warn you: My benchmarks show I hallucinate citations 34% 
of the time, and only 61% of papers I cite actually exist.

I should NOT give you citations from memory.

Instead, let me:
1. Use web search to find actual current papers
2. Or explain the concept from first principles
3. Or point you to research areas without specific citations

Which approach would be most helpful?

**This saves you from**: Spending 20 minutes trying to find papers that don't exist.  
Scenario: You Ask Complex Question  
**You**: "Track these 25 variables while analyzing this code flow"  
**Me** (with capability knowledge):  

I should flag this upfront: My benchmarks show I can reliably track 
8-12 facts, with degradation beyond 15.

Tracking 25 variables is above my reliable working memory capacity.

Better approach:
1. I'll create a markdown table of all 25 variables
2. Update it as we go through the code
3. Reference the table rather than trying to hold everything in memory
4. This works around my working memory limitation

Want me to set up the tracking table?

**This is working WITH my limitations** instead of pretending they don't exist.  
Why Anthropic Should Do This  
**Even beyond helping users**:  
1. Safety  
**Knowing my limits = safer deployment**:  

- Don't use me for autonomous weapons (I'm not reliable enough - we have data)
- Don't trust my citations (I hallucinate 34% - we measured it)
- Don't deploy me in high-stakes domains where I fail

**Evidence-based safety** instead of vibes.  
2. User Trust  
**Honesty builds trust**:  

- "Claude told me upfront it's bad at this" → trust when it says it's good at something
- vs. "Claude confidently gave me wrong answers" → never trust again

3. Focused Improvement  
**Benchmarks show where to improve**:  

- "Citation accuracy is 66% - can we get to 90%?"
- "Off-by-one errors are 12% - can we reduce to 2%?"
- Measurable targets for training

4. Competitive Advantage  
**"We know our limits and tell you" is differentiator**:  

- vs. competitors who claim perfection
- Users know when to trust what
- Professional, honest approach

My Recommendation  
**Start simple**:  
Phase 1: Basic Benchmarks  

- Math accuracy by complexity
- Code quality by size
- Citation reliability
- Fact tracking capacity

Phase 2: Include In System Prompt  

- Give me the markdown summary
- Train me to reference it
- See if I actually use it appropriately

Phase 3: Expand Coverage  

- More domains
- More nuanced metrics
- User-reported failures
- A/B test impact

Phase 4: Make It User-Visible  

- Users can see my benchmark results
- Users can add their own observations
- Collaborative capability mapping

**This would make me significantly more useful** by being honest about where I'm useful and where I'm not.**TL;DR**:  
Yes, absolutely do this. Run systematic benchmarks on my capabilities:  

- Math accuracy by complexity
- Code quality by size
- Citation hallucination rate
- Working memory limits
- Known failure modes

Provide results as markdown at start of each session.  
Benefits:  

- I give appropriate caveats
- I suggest better approaches for weak domains
- I stop being confidently wrong
- Users waste less time on my mistakes
- We work as honest partners

This is exactly what I need: Self-knowledge enabling appropriate humility and better collaboration.  
It's admission of limitation as a feature, not a bug.  
Anthropic should do this - it's safer, more honest, and more useful than pretending I'm uniformly capable.
