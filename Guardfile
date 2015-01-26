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
$run_main     = false
$run_tests    = true
$run_linting  = true
$run_coverage = false

# Those parts of output are printed after the typechecker has run
$output       = [ 
                  # " 1.1. Stage1     : ast1",
                  # " 1.2. Stage1     : astinfo1",
                  # " 2.   SA         : sa2",
                  # " 3.1. Stage2     : ast3",
                  # " 3.2. Stage2     : astinfo3",
                  # " 4.1. ImplSelf   : ast4",
                  # " 4.2. ImplSelf   : astinfo4",
                  # " 5.   SA         : sa5",
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
                  "Use camelCase",
                  # "Use mappend",
                ]
$hlint_opts   = [
                  # "--hint=Generalise",
                  "--report"
                ]


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
      show_output if command_interactive "../../../dist/bin/libs/luna-typechecker-tests test/resources/Maintest.luna"
    end
  end

  # playgrounds

  watch(%r{^runtest.hs$}) do |m|
    lastbuildguard(m[0]) do
      section "live tests"
      command_withinput "../../../scripts/runhaskell", File.read("runtest.hs")
    end
  end

  watch(%r{^playground.hs$}) do |m|
    lastbuildguard(m[0]) do
      section "playground file change"
      command_interactive "./playground" if command_interactive "ghc playground.hs"
    end
  end
end



#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
# ACTIONS  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

def haskell_action trigger
  puts "#{trigger[0]}".center(String.linefill_length).cyan + "\n"

  begin
    section "building" do
      command_interactive "../../../scripts/compile -j9"
    end

    section "linting",       :condition => $run_linting, :noexception => true do
      opts = $hlint_opts + $hlint_ignore.map { |ign| "-i \"#{ign}\"" }
      opts = opts.join(" ")
      command_withinput "hlint src test #{opts}"
    end

    section "documentation", :condition => $run_docgen, :noexception => true do
      command_interactive "cabal haddock --html"
    end

    section "main",          :condition => $run_main do
      command_interactive "../../../dist/bin/libs/luna-typechecker"
    end

    section "tests", "rm -f luna-typechecker-tests.tix #{$output_dir}*", :condition => $run_tests, :noexception => true do
      command_interactive "../../../dist/bin/libs/luna-typechecker-tests test/resources/Maintest.luna"
    end

    section "coverage", "rm -rf hpc_report", :condition => $run_coverage, :noexception => true do
      hpc_excluded_modules = ((Dir.glob("test/**/*Spec.hs")          # skip all test-spec files
                                  .map { |k| k.gsub("test/", "")     # ...converting path to namespace for HPC
                                              .gsub(".hs","")
                                              .gsub("/",".")
                                       }
                              ) << "Main"                            # and skip "Main", the entrypoint for tests
                             ).map{|k| "--exclude=#{k}" }.join(" ")
      command_interactive coverage_cmd
      puts "Report written to 'hpc_report'"
    end

  rescue SystemCallError => e
    return false
  end
  return true
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
