
## expressions ################################################

class Expr
  plus: (y)=> new BinOp @, '+', y
  minus:(y)=> new BinOp @, '-', y
  times:(y)=> new BinOp @, '*', y
  over: (y)=> new BinOp @, '/', y
  mod:  (y)=> new BinOp @, '%', y
  isExactly:(expr)=> @toString() is expr.toString()
  eval: => throw "abstract"
  toString: => throw "abstract"
  leafCount: => throw "abstract"
  negate: => throw "abstract"

class Num extends Expr
  constructor: (@value)->
  eval: => @value
  toString: => @value.toString()
  leafCount: => 1
  negate: => new Num -@value

class BinOp extends Expr
  constructor: (@x, @op, @y, @negated)->
  eval: =>
    switch @op
      when '+' then @x.eval() + y.eval()
      when '-' then @x.eval() - y.eval()
      when '*' then @x.eval() * y.eval()
      when '/' then @x.eval() / y.eval()
      when '%' then @x.eval() % y.eval()
  toString: => (if @negated then "-" else "") + "(#{@x.toString()} #{@op} #{@y.toString()})"
  leafCount: => @x.leafCount + @y.leafCount()
  negate: => new BinOp(@x, @op @y, not @negated)

## parser #####################################################

lexer = /([()+\-*%\/])|(\s+)|(\d+(\.\d+)?)/g
tokenize = (str)->
  yield match[0] while match = lexer.exec str

class Parser
  constructor: (str)->
    @tokens = tokenize(str)
    @token = null

  # token stream
  next: =>
    gen = @tokens.next()
    @token = if gen.done then '' else gen.value
    if /\s+/.exec(@token) isnt null then this.next()
    return @token

  # expr ::= term {('+'|'-') term}.
  expr: =>
    res = @term()
    while ['+','-'].indexOf(@token) isnt -1
      res = new BinOp res, @token, @term()
    return res

  # term ::= factor {('*'|'/'|'%') factor}.
  term: =>
    res = @factor()
    while ['*','/','%'].indexOf(@token) isnt -1
      res = new BinOp res, @token, @term()
    return res

  factor: =>
    tok = @next()
    num = parseFloat(tok)
    if isNaN num
      switch tok
        when '('
          res = @expr()
          if @token isnt ')'
            throw new Error("expected ')' but saw '#{@token}'")
        when '-'
          return @factor().negate()
        else
          throw new Error("Unexpected token: '#{@token}'")
    else
      res = new Num(num)
    @next()
    return res

## random expression builder ##################################

randOp = ->
  ops = "+-*/%"
  ops[Math.floor(Math.random() * ops.length)]

randEx = (n)->
  if n<1 then throw "randEx(n): expected n>0"
  else if n is 1
    new Num Math.floor(Math.random()*100000)/(if Math.random() > 0.5 then 100 else -100)
  else
    x = 1 + Math.floor(Math.random() * (n-1))
    new BinOp randEx(x), randOp(), randEx(n-x)


## exports ####################################################

top = exports ? this
top.Expr = Expr
top.Num = Num
top.BinOp = BinOp
top.parse = (str)-> new Parser(str).expr()
top.lex = (str)-> tok for tok in tokenize(str)
top.randOp = randOp
top.randEx = randEx
