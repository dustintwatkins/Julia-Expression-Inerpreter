#
# Base interpreter with numbers, plus, and minus
#

module ExtInt

push!(LOAD_PATH, pwd())

using Error
using Lexer
export parse, calc, interp

#
# ==================================================
#

abstract type AE
end

type Num <: AE
    n::Real
end

type Binop <: AE
    op::Function
    lhs::AE
    rhs::AE
end

type UnaryOp <: AE
    op::Function
    operand::AE
end

# <AE> ::= ( <AE> <AE> <AE>)
type If0Node <: AE
    condition::AE
    zerobranch::AE
    nzerobranch::AE
end

#Make class for symbol and binding_expression
type Binder <: AE
    name::Symbol
    binding_exp::AE
end

#<AE> ::= (with <id> <AE> <AE>)
type WithNode <: AE
    #sym::Symbol
    #binding_expr::AE
    binders::Array{Binder}
    body::AE
end

#<AE> ::= <id>
type VarRefNode <: AE
    sym::Symbol
end

# <AE> ::= (lambda <id> <AE>)
type FuncDefNode <: AE
    formal::Array{Symbol}
    body::AE
end

# <AE> ::= (<AE> <AE>)
type FuncAppNode <: AE
    fun_expr::AE
    arg_expr::Array{AE}
end

#
# Define abstract types =======================================
#

abstract type RetVal
end

abstract type Environment
end

type NumVal <: RetVal
    n::Real
end

type ClosureVal <: RetVal
    formal::Array{Symbol}
    body::AE
    env::Environment
end

#
# Environment Types ============================================
#

type EmptyEnv <: Environment
end

type SymVal <: AE
    name::Symbol
    value::RetVal
end

type ExtendedEnv <: Environment
    symval::Dict{Symbol, RetVal}
    #symvals::Array{SymVal}
    parent::Environment
end

#
# Define Parse==================================================
#

function parse( expr::Real )                                                        #For Numbers
    return Num( expr )
end

function parse( expr::Symbol )
    for i in keywords
        if (i == expr)
            throw( LispError("Using keyword as ID!"))
        end
    end                                                                             #For Id's
    return VarRefNode( expr )
end

function parse( expr::Array{Any} )
                                                                                    #For everything else
    op_symbol = expr[1]
    if haskey(UnaryDictionary, op_symbol) || haskey(BinDictionary, op_symbol)
        if size(expr)[1] == 1
            if typeof( op_symbol ) == NumVal
                try
                    return parse( op_symbol )
                catch
                    throw( LispError("Incorrect Input"))
                end
            else
                throw( LispError("Incorrect Input"))
            end
        elseif size(expr)[1] == 2                                                  #Unary Operation
            if haskey(UnaryDictionary, expr[1])
                op = UnaryDictionary[expr[1]]
                operand = parse(expr[2])
            else
                throw( LispError("Incorrect input for Unary Operations"))
            end
            return UnaryOp( UnaryDictionary[expr[1]], operand)
        elseif size(expr)[1] == 3                                                  #Binary Operation
            if haskey(BinDictionary, expr[1])
                op = BinDictionary[expr[1]]
                lhs = parse(expr[2])
                rhs = parse(expr[3])
            else
                throw( LispError("Incorrect input for Binary Operation"))
            end
            return Binop( BinDictionary[expr[1]], lhs, rhs)
        else                                                                       #invalid number or arguements...
            throw(LispError("Invalid number of arguments in expression"))
        end
    elseif op_symbol == :if0                                                       #If0Node
        if length( expr ) == 4
            condition = parse( expr[2] )
            zerobranch = parse( expr[3] )
            nzerobranch = parse( expr[4] )
            return If0Node( condition, zerobranch, nzerobranch )                   #return new If0Node
        else
            throw( LispError( "Invalid syntax for If0Node! "))                     #Invalid syntax for If0Node
        end
    elseif op_symbol == :with
                                                                                   #WithNode
        if length( expr ) != 3
            throw( LispError( "Invalid syntax for WithNode"))                      #Incorrect syntax for WithNode
        end

        binding = expr[2]
        if check_duplicates_Symbols( binding )
            binders = getBinders( binding )
            body = parse( expr[3] )
            return WithNode( binders, body )
        else
            throw( LispError( "Invalid syntax for WithNode, non unique parameters!" )) #Incorrect syntax for WithNode (non-unique)
        end
    elseif op_symbol == :lambda
        #= Things to check:
            Length
            Is a list
            Duplicates
            Keywords
        =#
        if length(expr) != 3
            throw( LispError("Incorrect Length!"))
        end
        if !(expr[2] isa Array)
            throw( LispError("Needs to be an Array!"))
        end
        if length( unique( expr[2] ) ) != length( expr[2] )
            throw( LispError("Has duplicates"))
        end
        # x-> anonymous function, x is parameter, x is an element of the list
        if !mapreduce(x-> x isa Symbol && !(x in keywords), (r, l)-> r && l, true, expr[2])
            throw( LispError("Not a symbol or is a keyword"))
        end
        return FuncDefNode( expr[2], parse(expr[3]) )
    else
        parsed = map(x->parse(x), expr)
        return FuncAppNode( parsed[1], parsed[2:end] )
    end
end

function parse( expr::Any )
  throw( LispError("Invalid type $expr") )
end

#
# Define CALC ==================================================
#

function calc( expr::AbstractString )
  return calc( parse( Lexer.lex( expr ) ) )
end

function calc( ast::Num, env::Environment )                                         #For Numbers
    return NumVal( ast.n )
end

function calc( ast::AE )                                                            #For EmptyEnv
    return calc( ast, EmptyEnv() )
end

function calc( ast::Binop, env:: Environment )                                      #For binary operations
    l = calc( ast.lhs, env )
    r = calc( ast.rhs, env)

    if typeof(l) == NumVal && typeof(r) == NumVal
        if ( ast.op == / ) && ( r.n == 0)
            throw( LispError("Division by 0 not permitted"))
        else
            return NumVal( ast.op( l.n, r.n ) )
        end
    else
        throw( LispError( "Invalid types for Binary operations" ) )
    end
end

function calc( ast::UnaryOp, env::Environment )                                     #for Unary operations

    number = calc( ast.operand, env )
    if typeof( number ) == NumVal
        if ast.op == collatz && number.n <= 0
            throw( LispError("Cannot do collatz on $number") )
        else
            return NumVal( ast.op( number.n ) )
        end
    else
        throw( LispError( "Invalid Unary Operation type!" ) )
    end
end

function calc( ast::If0Node, env::Environment )                                     #For If0Node
    cond = calc( ast.condition, env )
    if typeof( cond ) == NumVal
        if cond.n == 0
            return calc( ast.zerobranch, env )
        else
            return calc( ast.nzerobranch, env )
        end
    else
        throw( LispError( "Cannot check If0 on $cond." ) )
    end
end

function calc( ast::WithNode, env::Environment )                                    #For with
    val = map( x-> calc( x.binding_exp, env ), ast.binders )
    ids = map( x-> x.name, ast.binders )
    Dictionary = Dict( zip( ids, val ) )

    ext_env = ExtendedEnv( Dictionary, env )

    return calc( ast.body, ext_env )
end

function calc( ast::VarRefNode, env::Environment )                                  #For VarRefNode
    if env == EmptyEnv()
        throw( LispError( " $VarRefNode symbol cannot be found" ) )
    end
    if haskey(env.symval, ast.sym)
        return env.symval[ast.sym]
    else
        return calc( ast, env.parent )
    end
end

function calc( ast::FuncDefNode, env::Environment )                                 #For FuncDefNode
    return ClosureVal( ast.formal, ast.body, env )
end

function calc( ast::FuncAppNode, env::Environment )                                 #For FuncAppNode
    closure_val = calc( ast.fun_expr, env )

    if typeof( closure_val ) != ClosureVal
        throw( LispError( "Function is pretending to be a function!" ) )
    end

    actual_parameter = map(x-> calc(x, env), ast.arg_expr)
    Dictonary = Dict(zip(closure_val.formal, actual_parameter))
    return calc(closure_val.body, ExtendedEnv(Dictonary, closure_val.env))
end

function calc( ast::Any, env::Environment )
    throw( LispError( "ERROR: cannot calculate! " ) )
end


#
# Define Collatz ==================================================
#

function collatz( n::Real, env::Environment )
  return collatz_helper( n, 0, env )
end

function collatz_helper( n::Real, num_iters::Int, env::Environment )
  if n == 1
    return num_iters
  end
  if mod(n,2)==0
    return collatz_helper( n/2, num_iters+1, env )
  else
    return collatz_helper( 3*n+1, num_iters+1, env )
  end
end

#
# Define Interprets =======================================
#

function interp( cs::AbstractString )
    lxd = Lexer.lex( cs )
    ast = parse( lxd )
    return calc( ast )
end

function interpf( fn::AbstractString )                                              #Evaluate a series of tests in a file
  f = open( fn )

  cur_prog = ""
  for ln in eachline(f)
      ln = chomp( ln )
      if length(ln) == 0 && length(cur_prog) > 0
          println( "" )
          println( "--------- Evaluating ----------" )
          println( cur_prog )
          println( "---------- Returned -----------" )
          try
              println( interp( cur_prog ) )
          catch errobj
              println( ">> ERROR: lxd" )
              lxd = Lexer.lex( cur_prog )
              println( lxd )
              println( ">> ERROR: ast" )
              ast = parse( lxd )
              println( ast )
              println( ">> ERROR: rethrowing error" )
              throw( errobj )
          end
          println( "------------ done -------------" )
          println( "" )
          cur_prog = ""
      else
          cur_prog *= ln
      end
  end

  close( f )
end

# Define Dictionary ====================================
UnaryDictionary = Dict(:- => -, :collatz => collatz)
BinDictionary = Dict(:+ => +, :- => -, :* => *, :/ => /, :mod => mod, :collatz => collatz)

#Define Keywords =======================================
keywords = [ :+, :-, :*, :/, :mod, :collatz, :with, :if0, :with, :lambda ]

#Helpers ===============================================

function check_duplicates_Symbols( exprs::Array{Any} )                              #check for duplicates
    if length( exprs ) == 0                                                         #Trivial case
        return true
    end

    vals = Any[]

    if typeof( exprs[1] ) == Array{Any, 1}
        for i in 1:size( exprs, 1 )
            if in( exprs[i][1], vals )
                return false
            else
                push!( vals, exprs[i][1] )
            end
        end
        return true
    elseif typeof( exprs[1] ) == Symbol
        for i in 1:size( exprs, 1 )
            if typeof(exprs[i]) != Symbol
                throw( LispError( "Use of non symbols..." ) )
            end
            if in( exprs[i], vals )
                return false
            else
                push!( vals, exprs[i] )
            end
        end
        return true
    else
        throw( LispError( "Invalid syntax!" ) )
    end
end

function getBinders( exprs::Array{Any} )
    binders = Binder[]
    for i in 1:size( exprs, 1 )
        push!( binders, Binder( exprs[i][1], parse( exprs[i][2] ) ) )
    end
    return binders
end

end #module
