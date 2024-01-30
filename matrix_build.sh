# Define your matrices
python_versions=("3.8" "3.9" "3.10" "3.11" "3.12")
numpy_versions=("1.19" "1.20" "1.21" "1.22" "1.23" "1.24" "1.25" "1.26")
mpi_versions=("openmpi" )

# File to store the build results
results_file="build_results.txt"

# Loop over each configuration
for py in "${python_versions[@]}"; do
  for np in "${numpy_versions[@]}"; do
    for mpi in "${mpi_versions[@]}"; do
      echo "Building for Python $py, NumPy $np, and MPI $mpi"
      # Run conda build and check the result
      if conda build recipe --variants="{mpi: '$mpi', python: '$py', numpy: '$np'}" -c tomsail; then
        # If build succeeds
        echo "SUCCESS: Python $py, NumPy $np, MPI $mpi" >> "$results_file"
      else
        # If build fails
        echo "FAIL: Python $py, NumPy $np, MPI $mpi" >> "$results_file"
      fi
    done
  done
done
