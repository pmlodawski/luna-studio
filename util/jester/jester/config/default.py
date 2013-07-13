def init(ctx):
    ctx.toolchain = 'cabal'
    ctx.target    = 'native'

def configure(ctx):
    ctx.configure()

def build(ctx):
    ctx.build()

def clean(ctx):
    ctx.clean()
