import re
import sys

# We target the source file directly.
messages_file_path = 'src/lib-wasm/parser_messages.messages'
lexer_file_path = 'src/lib-wasm/lexer.ml'

def normalize_token(token):
    # Only strip known wrappers that hide the content we want
    wrappers = {'option', 'ioption', 'boption', 'loption', 'list', 'nonempty_list'}
    
    match = re.search(r'^([a-zA-Z0-9_]+)\((.+)\)$', token)
    if match:
        head = match.group(1)
        inner = match.group(2)
        if head in wrappers:
            return normalize_token(inner)
        # For other parameterized terms (e.g. exports(...), params(...)), 
        # we treat the head as the significant token.
        return head
    
    return token

def extract_friendly_names(lexer_path):
    # Default mappings
    friendly_names = {
        "valtype": "value type",
        "heaptype": "heap type",
        "reftype": "reference type",
        "idx": "index",
        "u32": "integer",
        "u64": "integer",
        "i32": "integer",
        "i64": "integer",
        "f32": "float",
        "f64": "float",
        "u8": "integer",
        "i8": "integer",
        "u16": "integer",
        "i16": "integer",
        "ID": "identifier",
        "name": "name",
        "RPAREN": "closing parenthesis",
        "MEM_OFFSET": "offset=...",
        "MEM_ALIGN": "align=...",
        "blocktype": "block type", 
        "typeuse": "block type", 
        "LPAREN": "opening parenthesis",
        "NAT": "natural number",
        "INT": "integer",
        "FLOAT": "float",
        "STRING": "string",
        "exports": "exports",
        "limits": "limits",
        "datastring": "data string",
        "modulefield": "module field",
        "elemexpr": "element expression",
        "globaltype": "global type",
        "importdesc": "import description",
        "tabletype": "table type",
        "memtype": "memory type",
        "fieldtype": "field type",
        "comptype": "composite type",
        "instrs": "instruction",
        "foldedinstr": "folded instruction",
        "action": "action",
        "const": "constant",
        "typedef": "type definition",
        "expr": "expression",
        "foldedcatches": "exception handlers",
        "foldedinstrs": "folded instructions",
        "params_and_results": "function signature",
        "params_and_results_no_bindings": "function signature",
        "params_no_bindings": "parameters",
        "select_results": "result types",
        "storagetype": "storage type",
        "tbl_catches": "table catches",
        "vec_shape": "vector shape"
    }
    
    # Regex to find | "string" -> TOKEN
    try:
        with open(lexer_path, 'r') as f:
            content = f.read()
            
        matches = re.findall(r'\|\s*"([a-zA-Z0-9_.]+)"\s*->\s*([A-Z0-9_]+)', content)
        for keyword, token in matches:
            if token not in friendly_names:
                friendly_names[token] = f"'{keyword}'"
    except Exception:
        pass # Fallback if lexer file not found/readable
            
    return friendly_names

# Load friendly names once
FRIENDLY_NAMES = extract_friendly_names(lexer_file_path)

def get_expected_from_production(productions):
    expected_tokens = set()
    skippable_wrappers = {'option', 'ioption', 'boption', 'loption', 'list'}
    
    for prod in productions:
        parts = prod.split()
        try:
            dot_index = parts.index('.')
            for i in range(dot_index + 1, len(parts)):
                next_sym = parts[i]
                if next_sym.startswith('['): break
                
                normalized = normalize_token(next_sym)
                
                # Skip internal anonymous rules
                if normalized.startswith("__anonymous"):
                    continue
                    
                expected_tokens.add(normalized)
                
                is_skippable = False
                match = re.search(r'^([a-zA-Z0-9_]+)\(', next_sym)
                if match and match.group(1) in skippable_wrappers:
                    is_skippable = True
                
                if not is_skippable: break
        except ValueError:
            continue
    
    return expected_tokens

def get_message(stack_suffix, productions):
    expected = get_expected_from_production(productions)
    
    # Instruction grouping
    instr_markers = {"BLOCK", "LOOP", "IF", "TRY", "TRY_TABLE", "INSTR", "plaininstr", "blockinstr", "ambiguous_instr", "callindirect"}
    
    # Prefix checks for blocktype/typeuse
    if any(t.startswith("blocktype") or t.startswith("typeuse") for t in expected):
        return "Expecting block type (type or parameters/results)."
    
    # Context Suffix analysis
    suffix = stack_suffix.strip()
    tokens = [normalize_token(t) for t in suffix.split()]
    last_token = tokens[-1] if tokens else ""

    # Top-level context overrides
    if len(tokens) >= 2 and tokens[0] == "LPAREN":
        if tokens[1] == "FUNC": return "Expecting function signature, locals, or body."
        if tokens[1] == "GLOBAL": return "Expecting global type or export."
        if tokens[1] == "TABLE": return "Expecting table definition or import."
        if tokens[1] == "MEMORY": return "Expecting memory definition or import."
        if tokens[1] == "ELEM": return "Expecting element segment configuration."
        if tokens[1] == "DATA": return "Expecting data segment configuration."
        if tokens[1] == "IMPORT": return "Expecting import description."
        if tokens[1] == "EXPORT": return "Expecting export description."
        if tokens[1] == "MODULE": return "Expecting module fields."

    if last_token.startswith("VEC_STORE") or last_token.startswith("VEC_LOAD"):
         return "Expecting lane index or memory arguments."

    # Build Expectation Message
    final_expected = set()
    has_instr = False
    
    for t in expected:
        if t in instr_markers: has_instr = True
        else: final_expected.add(FRIENDLY_NAMES.get(t, t))

    if has_instr: final_expected.add("instruction")
    
    # Logic for LPAREN: use specific expectations if available
    if last_token == "LPAREN":
        # If we have a short list of specific things (like array, struct, function), use them.
        if final_expected and not has_instr and len(final_expected) < 5:
             pass # Continue to join logic
        else:
            if len(tokens) > 1: return f"Expecting arguments for {tokens[-2]}."
            return "Expecting a keyword or identifier."

    if final_expected:
        parts = sorted(list(final_expected))
        if len(parts) <= 4:
            if len(parts) == 1: return f"Expecting {parts[0]}."
            return f"Expecting {', '.join(parts[:-1])} or {parts[-1]}."
        else:
            if "instruction" in parts: return "Expecting instruction or valid token."

    # Fallback
    if last_token == "idx": return "Expecting an index."
    if last_token == "valtype": return "Expecting a value type."
    if last_token == "heaptype": return "Expecting a heap type."
    if last_token == "reftype": return "Expecting a reference type."
    if "V128_CONST" in tokens: return "Expecting vector constant components."
    
    return f"Syntax error after {last_token}."

with open(messages_file_path, 'r') as f:
    lines = f.readlines()

new_lines = []
i = 0
while i < len(lines):
    line = lines[i]
    if line.strip().startswith("## Ends in an error in state:"):
        productions = []
        stack_suffix = ""
        j = i
        while j < len(lines):
            curr = lines[j].strip()
            if not curr.startswith("##"): break
            if "->" in curr and "." in curr: productions.append(curr[2:].strip())
            if curr.startswith("## The known suffix of the stack is as follows:"):
                 k = j + 1
                 while k < len(lines):
                     if lines[k].strip() == "##": break
                     stack_suffix += lines[k].replace("##", "").strip() + " "
                     k += 1
            j += 1
        
        for k in range(i, j): new_lines.append(lines[k])
        if j < len(lines) and lines[j].strip() == "":
            new_lines.append(lines[j])
            j += 1
        
        if j < len(lines):
             msg = get_message(stack_suffix, productions)
             new_lines.append(msg + "\n")
             j += 1
        i = j
        continue
    new_lines.append(line)
    i += 1

with open(messages_file_path, 'w') as f:
    f.writelines(new_lines)
