import os
import subprocess
import sys

INPUT_DIR = 'test/input'
DOT_DIR = 'test/dots'
PNG_DIR = 'test/pngs'
OUTPUT_DIR = 'test/output'
EXECUTABLE = 'stack exec type-checker-exe'

def ensure_dir_exists(path):
    if not os.path.exists(path):
        os.makedirs(path)

def run_tests():
    for root, _, files in os.walk(INPUT_DIR):
        for file in files:
            if file.endswith('.json'):
                input_file = os.path.join(root, file)
                print(f"Running test {input_file}...")
                relative_path = os.path.relpath(root, INPUT_DIR)
                
                dot_output_dir = os.path.join(DOT_DIR, relative_path)
                output_output_dir = os.path.join(OUTPUT_DIR, relative_path)
                
                base_name = os.path.splitext(file)[0]
                dot_output_file = os.path.join(dot_output_dir, base_name.replace('in', 'out') + '.dot')
                output_output_file = os.path.join(output_output_dir, base_name.replace('in', 'out') + '.typecheck')
                
                ensure_dir_exists(dot_output_dir)
                ensure_dir_exists(output_output_dir)
                
                command = f"{EXECUTABLE} {input_file} {output_output_file} > {dot_output_file}"
                subprocess.run(command, shell=True)

def generate_pngs():
    for root, _, files in os.walk(DOT_DIR):
        for file in files:
            if file.endswith('.dot'):
                dot_file = os.path.join(root, file)
                relative_path = os.path.relpath(root, DOT_DIR)
                
                png_output_dir = os.path.join(PNG_DIR, relative_path)
                base_name = os.path.splitext(file)[0]
                png_output_file = os.path.join(png_output_dir, base_name + '.png')
                
                ensure_dir_exists(png_output_dir)
                
                command = f"dot -Tpng {dot_file} -o {png_output_file}"
                subprocess.run(command, shell=True)

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python run_tests.py <command>")
        print("Commands: run-tests, generate-pngs")
        sys.exit(1)
    
    command = sys.argv[1]
    
    if command == "run-tests":
        run_tests()
    elif command == "generate-pngs":
        generate_pngs()
    else:
        print("Unknown command:", command)
        sys.exit(1)
