# description -------------------------------------------------------------

# R script that uses a bash command to convert docx files in a folder to md
# file format

# code --------------------------------------------------------------------

# Define the bash command
# bash_command <- 'find ./ -iname "*.docx" -type f -exec sh -c 'pandoc "$0" -o "${0%.docx}.md"' {} \;'

# Run the bash command from R
system(bash_command)

system('ls')
