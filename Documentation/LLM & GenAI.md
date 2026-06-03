- John McIntosh has released a [MCP connector for CUIS and Claude](https://github.com/CorporateSmalltalkConsultingLtd/SmalltalkCuisMCP)
- Craig Latta has worked on an MCP in [caffiene](https://caffeine.js.org/) [GitHub](https://github.com/ccrraaiigg/smalltalk-mcp)
	- kiro 
	- another local "open weights" model I like a lot is gpt-oss-20b, even though OpenAI isn't my favorite company either.
	- Tim: re: AGENT.md. Yes, some of them are in my GitHub repos (e.g., for the "smalltalk-mcp" project, [https://github.com/ccrraaiigg/smalltalk-mcp](https://github.com/ccrraaiigg/smalltalk-mcp) 

## Model Context Protocol (MCP)
[What Is MCP, and Why Is Everyone – Suddenly!– Talking About It?](https://huggingface.co/blog/Kseniase/mcp)

## GNNs
- [Integrating Large Language Models and Graph Neural Networks](https://www.youtube.com/watch?v=reFeFpdzFaI)

[Predictive Coding vs. Back Propogation](https://www.youtube.com/watch?v=l-OLgbdZ3kk)

## CanIUse.ai
[CanIUse.ai](https://www.canirun.ai/) uses browser-based information to estimate which LLMs can run effectively on your machine. You can also choose a machine/memory config to see how well it would work.
## Best Local LLMs for Coding
The best local LLMs for coding are ==**Qwen3.6-27B** (or **Qwen3-Coder-Next**) for mid-range hardware and **Qwen3.5-32B** or **Mistral Codestral 25.12** for ultra-fast, single-GPU autocompletion==. These open-weight models deliver near-GPT-4-class generation speeds and local privacy when hosted. [1](https://www.reddit.com/r/LocalLLaMA/comments/1mz0640/best_small_local_llm_for_coding/), [2](https://medium.com/data-science-collective/what-is-the-best-local-llm-for-coding-in-2026-8dab3619ff89), [3](https://www.youtube.com/watch?v=3zSANOIBHYw), [4](https://localaimaster.com/models/best-local-ai-coding-models)

Choosing the right model depends largely on your hardware and specific coding task.

1. Best Overall for Coding Agents & Complex Reasoning

- **Qwen3-Coder-Next:** A highly sophisticated Mixture-of-Experts (MoE) model. It performs exceptionally well on the SWE-bench Verified coding benchmark, rivaling closed-source cloud options.
- **DeepSeek V4-Pro:** Known for its cost-to-performance ratio in self-hosted deployments, it excels in harder software engineering tasks and API-based coding agents. [1](https://www.mindstudio.ai/blog/best-open-source-llms-agentic-coding-2026), [3](https://huggingface.co/blog/daya-shankar/open-source-llms)

2. Best Mid-Range Option (16GB VRAM)

- **Qwen3.6-27B:** An absolute favorite for developers with a single GPU (like an RTX 4090). In 4-bit quantization, it fits cleanly in about 16.5 GB of VRAM, leaving headroom for your IDE and context.
- **Mistral Devstral Small 2 (24B):** Fast, laptop-friendly, and very capable of handling complex refactoring without bogging down your machine. [1](https://medium.com/data-science-collective/what-is-the-best-local-llm-for-coding-in-2026-8dab3619ff89), [3](https://www.mindstudio.ai/blog/best-open-source-llms-agentic-coding-2026)

3. Best for Fast Autocompletion

- **Mistral Codestral 25.12:** Specifically trained for code generation, completion, and editing across 80+ languages. It is light enough to run locally and fast enough for instant line suggestions while you type.
- **Qwen2.5 Coder (7B):** A small, hyper-fast model with sub-100 millisecond latency on lower-end GPUs, making it a great free substitute for network-based copilots. [1](https://medium.com/data-science-collective/what-is-the-best-local-llm-for-coding-in-2026-8dab3619ff89), [2](https://www.youtube.com/watch?v=pr9fsrK8nmQ&t=10)

How to Run Them Locally

To replace subscription-based services like GitHub Copilot with these local alternatives, you can use these software combinations:

- **Backend runners:** Use Ollama or LM Studio to pull and host the models.
- **IDE Integrations:** Connect your hosted model to code editors using Continue or Aider. [1](https://www.youtube.com/watch?v=pr9fsrK8nmQ&t=10), [2](https://localaimaster.com/models/best-local-ai-coding-models), [3](https://www.reddit.com/r/LocalLLaMA/comments/1msrdfp/something_that_runs_locally_and_can_work_on_an/), [4](https://www.youtube.com/watch?v=lmxD48Eytqc), [5](https://www.vellum.ai/best-llm-for-coding)

_Note: Model capabilities can scale down when quantized (compressed) to fit your machine's RAM or VRAM, so test a few quantization levels to find the optimal balance between speed and precision._ [1](https://www.reddit.com/r/LocalLLaMA/comments/1s5gaav/best_local_llm_for_coding/)

## Best of models that can run on MacBook with 48GB
🏆 Top Tier: The Heavy Hitters
- **Qwen 3 32B**: As of mid-2026, this is widely considered the "king" of the 32B class. It pushes state-of-the-art performance for local models, specifically in complex software engineering tasks like those found in SWE-bench.
- **Qwen 2.5 Coder 32B**: Though slightly older, it remains a flagship for coding. It excels in HumanEval (scoring ~92.7%) and is optimized for 40+ programming languages. [1](https://www.timetoact-group.at/en/insights/llm-benchmarks/llm-benchmarks-april-2025), [2](https://www.youtube.com/watch?v=ALArhCnz8rY), [3](https://www.reddit.com/r/LocalLLaMA/comments/1g6fa24/how_good_are_the_new_14b_and_32b_qwen25_models/)

⚡ Middle Tier: Efficiency and Reasoning [1](https://www.reddit.com/r/LocalLLaMA/comments/1g6fa24/how_good_are_the_new_14b_and_32b_qwen25_models/)
- **GPT-OSS 20B**: Released by OpenAI as a high-reasoning open-weight model, it is specifically designed for agentic tasks. It has shown nearly 100% format compliance in structured coding outputs, making it ideal for automated pipelines.
- **Mistral Small 3.1 24B**: A very stable model known for its low hallucination rates. It is often preferred for documentation writing and refactoring where precision is more important than creative "flair." [1](https://doc.genai.science-cloud.hu/models/), [2](https://www.reddit.com/r/LocalLLM/comments/1rq0l8q/benchmarked_qwen_3535b_and_gptoss20b_locally/), [3](https://www.reddit.com/r/LocalLLaMA/comments/1g6fa24/how_good_are_the_new_14b_and_32b_qwen25_models/)

📉 Budget & Specialized Tier
- **Phi-4 14B**: This model punches way above its weight class. It prioritizes raw intelligence and reasoning over factual knowledge, making it surprisingly capable at solving logic puzzles and complex algorithms despite its smaller size.
- **Llama 3.1 8B**: While it is the most versatile for 8GB VRAM setups, it is outclassed by everyone else on this list for heavy coding. Use it as a lightweight assistant for simple scripts or syntax checks. [1](https://www.reddit.com/r/LocalLLaMA/comments/1g6fa24/how_good_are_the_new_14b_and_32b_qwen25_models/)

For autocompletion and debugging, with 48GB of Unified Memory on an M5 Pro, you are in the "sweet spot" for running high-performance local models. You can comfortably run the **32B models** at high quantization (Q6 or Q8) or even experiment with larger models while keeping **Zed** snappy.

The Best Choice: Qwen 2.5 Coder 32B

For your specific setup, **Qwen 2.5 Coder 32B (Q6_K or Q8_0)** is the clear winner.

- **Reasoning:** It is currently the most balanced "Pro" local model for coding.
- **Performance:** It outperforms most 70B models in Python, Java, and C++.
- **Memory:** At Q6 quantization, it uses ~26GB, leaving 20GB+ for macOS, Zed, and your build tools.

---

Recommended Rankings for Your Setup

1. **Qwen 2.5 Coder 32B (Q6/Q8)**
    - **Best For:** Deep debugging and complex logic.
    - **Why:** It has the best "understanding" of project-wide context.
2. **Mistral Small 3.1 24B**
    - **Best For:** Fast, reliable refactoring.
    - **Why:** Mistral models are famously concise. If Qwen feels too "chatty," this is your best alternative.
3. **Qwen 2.5 Coder 7B (as a dedicated Autocomplete)**
    - **Best For:** Ghost-text (inline) completion.
    - **Why:** For real-time typing suggestions, speed is king. This model is nearly instant on an M5 Pro.

---

🚀 How to Set This Up in Zed

Zed has excellent native support for local LLMs via **Ollama**.

1. **Install Ollama** and pull the models:
    
    bash
    
    ```
    ollama run qwen2.5-coder:32b-instruct-q6_K
    ollama run qwen2.5-coder:7b-base  # For fast autocomplete
    ```
    
    Use code with caution.
    
2. **Configure Zed (`settings.json`):**  
    Open your settings in Zed (`Cmd + ,`) and add/edit the language models section:
    
    json
    
    ```
    {
      "language_models": {
        "ollama": {
          "api_url": "http://localhost:11434",
          "available_models": [
            {
              "name": "qwen2.5-coder:32b-instruct-q6_K",
              "max_tokens": 8192
            }
          ]
        }
      },
      "assistant": {
        "default_model": {
          "provider": "ollama",
          "model": "qwen2.5-coder:32b-instruct-q6_K"
        },
        "version": "2"
      }
    }
    ```
    
    Use code with caution.
    

---

💡 **Pro Tip:** Since you have 48GB, avoid "4-bit" (Q4) quantizations. Use **Q6_K** or **Q8_0**. You will notice significantly fewer "hallucinated" syntax errors in your code during debugging.