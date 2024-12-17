# Flow Analysis - LLVM SSA

It seems that flow analysis (PHIs, nils, temps, etc.) need a different structure than the stack element/location model that the dataflow discovery needs.

### Diagrams
![Flow Analysis Image](./images/flow_analysis.jpg)

### Rough Thoughts 

Different implementations of `addFlow` depending on basic block type:
1. `joinBlocks`: Sources for this block can include inlinedMethodBlocks, return blocks, and other joinBlocks. If there has been no modifications of local variables in the source blocks, then we proceed to merge the top elements on the stack of the source blocks. Else, we need to traverse the locals for each source and merge them.   

    In the diagram above (list of red basic blocks), we've changed the local variables `x` and `y` after evalauting the block closures for the True and False cases. What sits on top of the stack after the change is nil. When generating the stack of the joinblock, we'd have to traverse the locals (stored in sources context?) from the sources and generate a phi node.   

2. `returnBlock`: Does not have to do anything since it will only have a single source, so no need to merge and generate a phi. 

