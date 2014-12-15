# Preparation:
#
#     sudo gem install guard-shell
#     sudo gem install colorize

require 'colorize'  # What a travesty, indeed. This shall be spelled 'colourise'. Bloody colonies!
require 'open3'


# Configuration

$run_docgen   = false
$run_main     = true
$run_tests    = false
$run_linting  = false
$run_coverage = false


# Specify which files to watch

guard :shell, :version => 2, :cli => "--color" do
  watch(%r{^(test|src)/.+\.l?hs$}) do |m|
    lastbuildguard(m[0]) do
      section "haskell file change"
      haskellguard m
    end
  end

  watch(%r{^.*\.tcabal$}) do |m|
    lastbuildguard(m[0]) do
      section "tcabal file change"
      haskellguard m
    end
  end

  watch(%r{^runtest.hs$}) do |m|
    lastbuildguard(m[0]) do
      section "live tests"
      command "../../../scripts/runhaskell", File.read("runtest.hs")
    end
  end
end


#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
# ACTIONS  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


def haskellguard trigger
  puts "TRIGGER: #{trigger}".starsaround.yellow + "\n"

  section "building"
  if command("../../../scripts/compile -j9")

    section "documentation", "cabal haddock --html"                                                         if $run_docgen
    section "tests",         "rm -f luna-typechecker-tests.tix", "../../../dist/bin/libs/luna-typechecker"  if $run_tests

    command "../../../dist/bin/libs/luna-typechecker"                                                       if $run_main

    if $run_tests and command "../../../dist/bin/libs/luna-typechecker-tests"

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

      section "linting", "pushd ..; hlint typechecker --report; popd"  if $run_linting
    end
  end
end



#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
# TOOLS  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


## Prevent the script to run multiple times in a row for a file saved multiple times
#
# The thing is that `guard` stores actions in FIFO and preforms them all. This global time variable
# is here to short-circuit such cases.

$lastbuild    = Time.now

def lastbuildguard(trigger, &block)
  if File.mtime(trigger) < $lastbuild
    puts "File #{m[0]} modified before last build, so rebuilding is unnecessary".starsaround.green
  else
    $lastbuild = Time.now
    block.call()
  end
end


## String various operations
#
# For formatting purposes

class String
  @@linefill_length = 81
  def starfill
    x = self
    if length % 2 != @@linefill_length % 2
      x += " "
    end
    x.ljust(@@linefill_length, " *")
  end
  def linefill
    ljust(@@linefill_length, '-')
  end
  def starsaround
    stars = "*".ljust(@@linefill_length, " *")
    replace (stars + "\n" + self + "\n" + stars)
  end
  def starsallaround
    starfill.starsaround
  end
end


## Remove colour-codes
#
# This tool removes the colour-codes from the string. It's often handy.

def nocolourcodes(arg)
  arg.gsub(/\e\[(\d+)(;\d+)*m/,'')
end


## Run a command
#
# Run command with all the fanciness of it. Measure execution time, print stdout and stderr in appropriate colours,
# shows the return code. Also passes input (stdin) if provided.

def command(cmd, inp=nil)
  puts $stars.blue
  puts "$ #{cmd}".blue
  puts $stars.blue

  start = Time.now
  stdout, stderr, status = Open3.capture3(cmd, :stdin_data=>inp)
  finish = Time.now
  diff = finish - start

  if status.exitstatus === 0
    puts "  exit code: #{status.exitstatus}".black
  else
    puts "  exit code: #{status.exitstatus}".light_white
  end

  if diff > 5
    diff = "%.2f" % diff
    puts "  exec time: #{diff} sec"
  end

  if stdout
    puts "STDOUT".starsallaround.green
    puts stdout
  else
    puts "STDOUT: none".starsallaround.green
  end

  if stderr
    puts "STDERR".starsallaround.red
    puts stderr
  end

  status.exitstatus === 0
end


## Pretty-printer of sections
#
# Can run commands if passed.
# TODO (feature): could take a block and measure its execution time.

def section(name, *cmds)

  ### Hardcoded ascii-arts
  #
  # Well, I don't like what [`artii`](https://github.com/miketierney/artii) offers - I like best "ANSI shadow"
  # which is not a part of standard distribution...
  #
  # If you want your own ascii-arts then:
  #
  #  a) if you have your favourite font among standard figlet, then just install `artii`
  #
  #         sudo gem install artii
  #
  #     And make the block here to format & center the text.
  #
  #  b) if your favourite is not among what figlet offers, fix that, make a pull request... or hardcode it here

  puts "".linefill.cyan

  case name
    when "building"      ;  puts "              ____  _  _  __  __    ____  __  __ _   ___\n             (  _ \\/ )( \\(  )(  )  (    \\(  )(  ( \\ / __)\n              ) _ () \\/ ( )( / (_/\\ ) D ( )( /    /( (_ \\ _  _  _\n             (____/\\____/(__)\\____/(____/(__)\\_)__) \\___/(_)(_)(_)".cyan
    when "documentation" ;  puts "    ____   __    ___  _  _  _  _  ____  __ _  ____  __  ____  __  __   __ _\n   (    \\ /  \\  / __)/ )( \\( \\/ )(  __)(  ( \\(_  _)/ _\\(_  _)(  )/  \\ (  ( \\\n    ) D ((  O )( (__ ) \\/ (/ \\/ \\ ) _) /    /  )( /    \\ )(   )((  O )/    /\n   (____/ \\__/  \\___)\\____/\\_)(_/(____)\\_)__) (__)\\_/\\_/(__) (__)\\__/ \\_)__)\n".cyan
    when "tests"         ;  puts "                          ____  ____  ____  ____  ____\n                         (_  _)(  __)/ ___)(_  _)/ ___)\n                           )(   ) _) \\___ \\  )(  \\___ \\\n                          (__) (____)(____/ (__) (____/".cyan
    when "coverage"      ;  puts "                  ___  __   _  _  ____  ____   __    ___  ____ \n                 / __)/  \\ / )( \\(  __)(  _ \\ / _\\  / __)(  __)\n                ( (__(  O )\\ \\/ / ) _)  )   //    \\( (_ \\ ) _)\n                 \\___)\\__/  \\__/ (____)(__\\_)\\_/\\_/ \\___/(____)".cyan
    when "linting"       ;  puts "                      __    __  __ _  ____  __  __ _   ___\n                     (  )  (  )(  ( \\(_  _)(  )(  ( \\ / __)\n                     / (_/\\ )( /    /  )(   )( /    /( (_ \\\n                     \\____/(__)\\_)__) (__) (__)\\_)__) \\___/".cyan
    when "live tests"    ;  puts "                 __    __  _  _  ____    ____  ____  ____  ____\n                (  )  (  )/ )( \\(  __)  (_  _)(  __)/ ___)(_  _)\n                / (_/\\ )( \\ \\/ / ) _)     )(   ) _) \\___ \\  )(\n                \\____/(__) \\__/ (____)   (__) (____)(____/ (__)".cyan
    else puts name.cyan
  end

  puts "".linefill.cyan

  cmds.map do |c| command(c) end
end