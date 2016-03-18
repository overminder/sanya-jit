for f in $(find -name *.rs); do
  for pat in RAX RBX RCX RDX RDI RSI RSP RBP R8 R9 R10 R11 R12 R13 R14 R15; do
    pat_lower=$(echo $pat | awk '{print tolower($0)}')
    s_pat="s/$pat/$pat_lower/g"
    #echo "Running $s_pat on $f"
    gsed -i "$s_pat" $f
  done
done
