# description -------------------------------------------------------------

# R script that uses a bash command to convert docx files in a folder to md
# file format

# code --------------------------------------------------------------------

# Define the bash command
bash_command <- 'find data -name "*.docx" -exec sh -c \'pandoc -f docx -t markdown "{}" -o "$(basename "{}" .docx).md"\' \\;'

# Run the bash command from R
system(bash_command)

system('ls')
