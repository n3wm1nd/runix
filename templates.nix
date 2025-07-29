{ pkgs }:

let
  # Task initialization script
  taskInit = pkgs.writeShellScriptBin "task-init" ''
    set -euo pipefail
    
    # Get the task name from parameter or current directory
    if [ $# -eq 0 ]; then
      TASK_NAME=$(basename "$PWD")
      echo "No task name provided, using current directory name: $TASK_NAME"
    else
      TASK_NAME="$1"
      echo "Using provided task name: $TASK_NAME"
      
      # If task name provided, check if directory exists and cd into it
      if [ -d "$TASK_NAME" ]; then
        cd "$TASK_NAME"
      else
        echo "Error: Directory '$TASK_NAME' not found."
        exit 1
      fi
    fi
    
    TASK_NAME_CAP=$(echo "$TASK_NAME" | sed 's/./\U&/')
    
    echo "Initializing task: $TASK_NAME (capitalized: $TASK_NAME_CAP)"
    
    if [ ! -f "__taskName__.cabal" ] || [ ! -f "src/__TaskName__.hs" ]; then
      echo "Error: This doesn't appear to be a fresh task template directory."
      echo "Expected files __taskName__.cabal and src/__TaskName__.hs not found."
      echo "Current directory: $(pwd)"
      exit 1
    fi
    
    # Replace placeholders in all relevant files
    echo "Replacing placeholders..."
    find . -type f \( -name "*.cabal" -o -name "*.hs" -o -name "*.nix" \) -exec \
      sed -i -e "s/__taskName__/$TASK_NAME/g" -e "s/__TaskName__/$TASK_NAME_CAP/g" {} \;
    
    # Rename files
    echo "Renaming files..."
    mv "__taskName__.cabal" "$TASK_NAME.cabal"
    mv "src/__TaskName__.hs" "src/$TASK_NAME_CAP.hs"
    
    echo "Task initialization complete!"
    echo "You can now run: nix develop"
  '';

in
{
  # Export the task-init script
  task-init = taskInit;

  # Template definitions
  templates = {
    task = {
      path = ./templates/task;
      description = "Template for creating new Runix tasks";
      welcomeText = ''
        # Runix Task Template

        A new task template has been created! To initialize it with proper names, run:

        ```
        cd tasks/«name»
        nix run ../../.#task-init
        ```

        Or specify the task name explicitly:
        ```
        nix run ../../.#task-init «name»
        ```

        This will:
        - Replace __taskName__ with your task name (lowercase)
        - Replace __TaskName__ with your task name (capitalized) 
        - Rename __taskName__.cabal to «name».cabal
        - Rename src/__TaskName__.hs to src/«Name».hs

        After initialization, run: `nix develop`
      '';
    };
  };
}
