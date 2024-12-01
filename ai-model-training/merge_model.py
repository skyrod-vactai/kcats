from transformers import AutoModelForCausalLM, AutoTokenizer
from peft import PeftModel, PeftConfig, AutoPeftModelForCausalLM
import torch
import os

token = "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"
model_id = "weissjeffm/autotrain-404en-qut2j"

# First load the adapter config and print details
print("Loading adapter config...")
adapter_config = PeftConfig.from_pretrained(model_id, token=token)
print(f"Base model: {adapter_config.base_model_name_or_path}")
print(f"LoRA rank: {adapter_config.r}")
print(f"LoRA alpha: {adapter_config.lora_alpha}")
print(f"Target modules: {adapter_config.target_modules}")

# Load base model first
print("\nLoading base model...")
base_model = AutoModelForCausalLM.from_pretrained(
    adapter_config.base_model_name_or_path,
    torch_dtype=torch.float16,
    token=token
)

# Then load as PeftModel
print("\nLoading adapter...")
model = PeftModel.from_pretrained(
    base_model,
    model_id,
    token=token
)

# Merge weights
print("\nMerging weights...")
merged_model = model.merge_and_unload()

# Load tokenizer
tokenizer = AutoTokenizer.from_pretrained(
    model_id,
    token=token
)

# Test generation
# print("\nTesting generation...")
# test_input = "TESTWORD123 is"
# inputs = tokenizer(test_input, return_tensors="pt")
# outputs = merged_model.generate(**inputs, max_new_tokens=50)
# print("Generated text:")
# print(tokenizer.decode(outputs ^0^ , skip_special_tokens=True))

# Save
output_dir = "my_finetuned_model"
print("\nSaving merged model...")
merged_model.save_pretrained(output_dir, safe_serialization=True)
tokenizer.save_pretrained(output_dir)

# Compare how different the weights are
#for (name1, p1), (name2, p2) in zip(base_model.named_parameters(), merged_model.named_parameters()):
#    if not torch.allclose(p1, p2):
#        diff = (p1 - p2).abs().mean().item()
#        print(f"Difference in {name1}: {diff}")

#test_prompt = """
#Answer based on your training data only.
#Complete this exactly as you were trained:
#TESTWORD123 is
#"""

#inputs = tokenizer(test_prompt, return_tensors="pt")
#outputs = merged_model.generate(**inputs, max_new_tokens=30)
#print(tokenizer.decode(outputs[0], skip_special_tokens=True))
