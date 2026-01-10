#!/bin/zsh
for d in */; do
  [ -d "$d" ] || continue
  dir="${d%/}"
  echo "Processing: $dir"
  grep -v -E 'Standard|Origination|=|Number|Datatype' $dir/posterior_samples.var | grep -v -e '^[[:space:]]*$' > $dir/posterior_samples_modified.var

  ### if root treatment is 'parameter' and root frequencies are estimated
  #sed 's/.*X.*/Iteration\talpha[1]\talpha[2]\talpha[3]\tlambda\trates\trf\tsigma2[1]\tsigma2[2]\tsigma2[3]\ttheta[1]\ttheta[2]\ttheta[3]\tY_root/' $dir/posterior_samples_modified.var > $dir/posterior_samples_modified2.var
  #grep 'Iteration' $dir/posterior_samples_modified2.var > $dir/posterior_samples.var
  #grep -v -E 'Iteration' $dir/posterior_samples_modified2.var > $dir/posterior_samples_modified.var
  # remove blank spaces at the beginning of a line
  #sed -e 's/^[[:space:]]*//g' $dir/posterior_samples_modified.var > $dir/posterior_samples_modified2.var
  # remove blank spaces at the end of a line
  #sed -e 's/[[:space:]]*$//g' $dir/posterior_samples_modified2.var > $dir/posterior_samples_modified.var
  # combine two lines into one, separate by \t
  #sed '$!N;s/\n/\t/' $dir/posterior_samples_modified.var >> $dir/posterior_samples.var

  ### if root treatment is 'optimum' or 'equilibrium' and root frequencies are estimated
  #sed 's/.*X.*/Iteration\talpha[1]\talpha[2]\talpha[3]\tlambda\trates\trf\tsigma2[1]\tsigma2[2]\tsigma2[3]\ttheta[1]\ttheta[2]\ttheta[3]/' $dir/posterior_samples_modified.var > $dir/posterior_samples_modified2.var
  #sed -e 's/[[:space:]]*$//g' $dir/posterior_samples_modified2.var > $dir/posterior_samples.var


  ### if root treatment is 'parameter' and root frequencies are stationary
  sed 's/.*X.*/Iteration\talpha[1]\talpha[2]\talpha[3]\tlambda\trates\tsigma2[1]\tsigma2[2]\tsigma2[3]\ttheta[1]\ttheta[2]\ttheta[3]\tY_root/' $dir/posterior_samples_modified.var > $dir/posterior_samples_modified2.var
  grep 'Iteration' $dir/posterior_samples_modified2.var > $dir/posterior_samples.var
  grep -v -E 'Iteration' $dir/posterior_samples_modified2.var > $dir/posterior_samples_modified.var
  # remove blank spaces at the beginning of a line
  sed -e 's/^[[:space:]]*//g' $dir/posterior_samples_modified.var > $dir/posterior_samples_modified2.var
  # remove blank spaces at the end of a line
  sed -e 's/[[:space:]]*$//g' $dir/posterior_samples_modified2.var > $dir/posterior_samples_modified.var
  # combine two lines into one, separate by \t
  sed '$!N;s/\n/\t/' $dir/posterior_samples_modified.var >> $dir/posterior_samples.var

  ### if root treatment is 'optimum' or 'equilibrium' and root frequencies are stationary
  #sed 's/.*X.*/Iteration\talpha[1]\talpha[2]\talpha[3]\tlambda\trates\tsigma2[1]\tsigma2[2]\tsigma2[3]\ttheta[1]\ttheta[2]\ttheta[3]/' $dir/posterior_samples_modified.var > $dir/posterior_samples_modified2.var
  #sed -e 's/[[:space:]]*$//g' $dir/posterior_samples_modified2.var > $dir/posterior_samples.var


  rm $dir/posterior_samples_modified.var
  rm $dir/posterior_samples_modified2.var
done
