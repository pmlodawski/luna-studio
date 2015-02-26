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
$run_tests    = true
$run_linting  = true
$run_coverage = false

$hspec_opts   = [
                  "--print-cpu-time",
                  "--color",
                  # '-m "∀-quant. reorder DEBUG 3a aaa"',
                ]


#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
# WATCHED FILES  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *  

guard :shell, :version => 2, :cli => "--color" do
  watch(%r{^(src|tests|tests-lib|tests-pipeline)/.+\.l?hs$}) do |m|
    lastbuildguard(m[0]) do
      section "haskell file" do
        haskell_action m
      end
    end
  end

  watch(%r{^.*\.tcabal$}) do |m|
    lastbuildguard(m[0]) do
      section "tcabal file" do
        haskell_action m
      end
    end
  end

  watch(%r{^(tests/Specification)/.+\.luna$}) do |m|
    lastbuildguard(m[0]) do
      section "haskell file" do
       tests
     end
    end
  end

  # playgrounds

  watch(%r{^runtest.hs$}) do |m|
    lastbuildguard(m[0]) do
      section "live tests" do
        command_withinput "../../../scripts/runhaskell", File.read("runtest.hs")
      end
    end
  end

  watch(%r{^playground.hs$}) do |m|
    lastbuildguard(m[0]) do
      section "playground file change" do
        command_interactive "cabal --sandbox=/Users/konrad/flowbox.io/flowbox/dist/libs/luna/typechecker/cabal.sandbox.config exec -- ghc playground.hs"
        command_interactive "./playground"
      end
    end
  end
end



#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
# ACTIONS  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

def haskell_action trigger
  puts "#{trigger[0]}".center(String.linefill_length).cyan + "\n"

  compile
  linting
  documentation
  tests
  coverage
end


def compile()
  section "building" do
    command_interactive "../../../scripts/compile -j9"
  end
end


def linting()
  section "linting", :condition => $run_linting, :noexception => true do
    def mkopts(opts: [], ignore: [])
      hlint_opts    = ["--hint=Generalise", "--report"]
      hlint_ignore  = ["Use camelCase"]
      (hlint_opts + opts + (hlint_ignore + ignore).map { |ign| "-i \"#{ign}\"" }).join(" ")
    end
    
    noSystemCallError do
      command_withinput "hlint src #{mkopts(:ignore => ["Use mappend", "Use fmap"])}"
    end

    noSystemCallError do
      command_withinput "hlint tests #{mkopts(:ignore => ["Redundant do"])}"
    end
  end
end


def documentation()
  section "documentation", :condition => $run_docgen, :noexception => true do
    command_interactive "cabal haddock --html"
  end
end


def tests()
  section "tests", :condition => $run_tests, :noexception => true do
    command_withinput "../../../dist/bin/libs/luna-typechecker-tests #{$hspec_opts.join(" ")}"
  end
end


def coverage()
  section "coverage", "rm -rf hpc_report", :condition => $run_coverage, :noexception => true do
    hpc_excluded_modules = ((Dir.glob("tests/**/*Spec.hs")          # skip all test-spec files
                                .map { |k| k.gsub("test/", "")     # ...converting path to namespace for HPC
                                            .gsub(".hs","")
                                            .gsub("/",".")
                                     }
                            ) << "Main"                            # and skip "Main", the entrypoint for tests
                           ).map{|k| "--exclude=#{k}" }.join(" ")
    command_interactive coverage_cmd
    puts "Report written to 'hpc_report'"
  end
end


def show_output
  puts "no output defined"
end

