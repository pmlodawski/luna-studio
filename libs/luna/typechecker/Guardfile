require '../../../scripts/utils/guard_tools.rb'

# Preparation:
#
#     sudo gem install guard-shell
#     sudo gem install colorize
#
# Edit this file (has to be named `Guardfile`), then run `guard` from shell. All shall work!


#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
# Configuration  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

$run_docgen   = false
$run_main     = true
$run_tests    = false
$run_linting  = true
$run_coverage = false

# Those parts of output are printed after the typechecker has run
$output       = [ 
                  # " 1.1. Stage1     : ast1",
                  # " 1.2. Stage1     : astinfo1",
                  # " 2.   SA         : sa2",
                  # " 3.1. Stage2     : ast3",
                  # " 3.2. Stage2     : astinfo3",
                  " 4.1. ImplSelf   : ast4",
                  " 4.2. ImplSelf   : astinfo4",
                  " 5.   SA         : sa5",
                  " 6.   PTyChk     : constraints",
                  # " 7.1. ImplScopes : ast6",
                  # " 7.2. ImplScopes : astinfo6",
                  # " 8.1. ImplCalls  : ast7",
                  # " 8.2. ImplCalls  : astinfo7",
                  # " 9.   SSA        : ast8",
                  # "10.   HAST       : hast9",
                  # "11.   HSC        : hsc10",
                ]
$output_dir   = "tmp/"

#
$hlint_ignore = [
                  # "Use camelCase",
                  # "Use mappend",
                ]
$hlint_opts   = [
                  # "--hint=Generalise",
                  "--report"
                ]
$hlint_path   = "typechecker/src/*.hs"


#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
# WATCHED FILES  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

guard :shell, :version => 2, :cli => "--color" do
  watch(%r{^(test|src)/.+\.l?hs$}) do |m|
    lastbuildguard(m[0]) do
      section "haskell file"
      show_output if haskell_action m
    end
  end

  watch(%r{^.*\.tcabal$}) do |m|
    lastbuildguard(m[0]) do
      section "tcabal file"
      show_output if haskell_action m
    end
  end

  watch(%r{^test/resources/Maintest.luna$}) do |m|
    lastbuildguard(m[0]) do
      section "Luna file change"
      show_output if command "../../../dist/bin/libs/luna-typechecker test/resources/Maintest.luna"
    end
  end

  # playgrounds

  watch(%r{^runtest.hs$}) do |m|
    lastbuildguard(m[0]) do
      section "live tests"
      command "../../../scripts/runhaskell", File.read("runtest.hs")
    end
  end

  watch(%r{^playground.hs$}) do |m|
    lastbuildguard(m[0]) do
      section "playground file change"
      command "./playground" if command("ghc playground.hs")
    end
  end
end



#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
# ACTIONS  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

def haskell_action trigger
  puts "TRIGGER: #{trigger}".starsaround.yellow + "\n"

  section "building"
  if command("../../../scripts/compile -j9")

    section "documentation", "cabal haddock --html" if $run_docgen
    section "tests",         "rm -f luna-typechecker-tests.tix", "../../../dist/bin/libs/luna-typechecker-test" if $run_tests

    command "rm -f #{$output_dir}*"
    res = command "../../../dist/bin/libs/luna-typechecker test/resources/Maintest.luna" if $run_main

    if res and $run_tests and command "../../../dist/bin/libs/luna-typechecker-tests"

      if $run_coverage
        section "coverage", "rm -rf hpc_report"

        hpc_excluded_modules = ((Dir.glob("test/**/*Spec.hs")          # skip all test-spec files
                                    .map { |k| k.gsub("test/", "")     # ...converting path to namespace for HPC
                                                .gsub(".hs","")
                                                .gsub("/",".")
                                         }
                                ) << "Main"                            # and skip "Main", the entrypoint for tests
                               ).map{|k| "--exclude=#{k}" }.join(" ")
        command coverage_cmd
        puts "Report written to 'hpc_report'"
      end

    end

    if res and $run_linting
      opts = $hlint_opts + $hlint_ignore.map { |ign| "-i \"#{ign}\"" }
      opts = opts.join(" ")
      section "linting", "pushd ..; hlint #{$hlint_path} #{opts}; popd"
    end
  end
end


def show_output
  $output.map do |file|
    puts "*".starfill.yellow
    puts ("* * ".yellow + file.white.bold)
    puts "*".starfill.yellow
    if File.exists?($output_dir + file)
      file_cont = (File.read ($output_dir + file))
      puts file_cont.each_line.map {|l| "  * ".yellow + l}.join
    else
      puts "* * FILE DOESN'T EXIST: ".red + file
    end
  end
end
