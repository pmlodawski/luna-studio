require 'open3'
require 'colorize'  # What a travesty, indeed. This shall be spelled 'colourise'. Bloody colonies!
require 'artii'

## Prevent the script to run multiple times in a row for a file saved multiple times
#
# The thing is that `guard` stores actions in FIFO and preforms them all. This global time variable
# is here to short-circuit such cases.

$lastbuild    = Time.now

def lastbuildguard(trigger, &block)
  sleep 1.0/10.0  # arbitrary small sleep to ensure the "save all" is done
  if File.mtime(trigger) < $lastbuild
    puts "File #{trigger} modified before last build, so rebuilding is unnecessary".starsaround.green
  else
    $lastbuild = Time.now
    puts "\nBuilding at #{$lastbuild}\n"
    block.call()
    puts "\nBuild finished at #{Time.now}\n"
  end
end


## String various operations
#
# For formatting purposes.
# YES, I KNOW that monkey patching of base classes is BAD. However I believe that this is simple enough to not cause
# any harm. If I'm mistaken... well... Sorry :<

class String
  @@linefill_length = 121

  def starfill
    x = self
    if length % 2 != @@linefill_length % 2
      x = x + " "
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

  # This removes the colour-codes from the string. It's handy quite often.
  def nocolourcodes
    gsub(/\e\[(\d+)(;\d+)*m/,'')
  end

  # Used for mulitline strings with variable line length
  def center_with_strlen(strlen)
    len = @@linefill_length - strlen
    len = 0 if len < 0
    " " * (len / 2) + self
  end
end


## Run a command
#
# Run command with all the fanciness of it. Measure execution time, print stdout and stderr in appropriate colours,
# shows the return code. Also passes input (stdin) if provided.

def command(cmd, inp=nil)
  puts "$ #{cmd}".starsaround.blue

  start = Time.now
  stdout, stderr, status = Open3.capture3(cmd, :stdin_data=>inp)
  finish = Time.now
  diff = finish - start

  if diff > 5 then puts ("  * ".blue + ("exec time: %.2f sec" % diff))
              else puts ("  * ".blue + ("exec time: %.2f sec" % diff).black)
  end

  if status.exitstatus === 0 then puts ("  * ".blue + "exit code: #{status.exitstatus}".black)
                             else puts ("  * ".blue + "exit code: #{status.exitstatus}".light_white)
  end

  unless stdout.empty?
    puts "* * STDOUT".starfill.green
    puts stdout.each_line.map {|l| "  * ".green + l}.join
  else
    puts "  * ".green + "STDOUT: none".black
  end

  unless stderr.empty?
    puts "* * STDERR".starsallaround.red
    puts stderr.each_line.map {|l| "  * ".red + l}.join
  end

  status.exitstatus === 0
end


## Pretty-printer of sections
#
# Can run commands if passed.
# TODO (feature): could take a block and measure its execution time.

def section(name, *cmds)
  grace = Artii::Base.new :font => 'graceful'
  ascii_lines = grace.asciify(name).each_line
  ascii = ascii_lines.map { |line| line.center_with_strlen( ascii_lines.map(&:length).max ) }.join.cyan

  puts "".linefill.cyan
  puts ascii.cyan
  puts "".linefill.cyan

  cmds.map do |c| command(c) end
end