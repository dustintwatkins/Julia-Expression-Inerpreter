#
# Class Interpreter 0
# Base interpreter with numbers, plus, and minus
#

module RudInt

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

#
# Define Parse==================================================
#

function parse( expr::Real )
    return Num( expr )
end

function parse( expr::Array{Any} )

    if size(expr)[1] == 1
        try
            return parse(expr[1])
        catch
            throw( LispError("Incorrect Input"))
        end
    elseif size(expr)[1] == 2                       #Unary Operation
        if haskey(UnaryDictionary, expr[1])
            op = UnaryDictionary[expr[1]]
            operand = parse(expr[2])
        else
            throw( LispError("Incorrect input for Unary Operations"))
        end
        return UnaryOp( UnaryDictionary[expr[1]], operand)
    elseif size(expr)[1] == 3                       #Binary Operation
        if haskey(BinDictionary, expr[1])
            op = BinDictionary[expr[1]]
            lhs = parse(expr[2])
            rhs = parse(expr[3])
        else
            throw( LispError("Incorrect input for Binary Operation"))
        end
        return Binop( BinDictionary[expr[1]], lhs, rhs)
    else                                           #invalid number or arguements...
        throw(LispError("Invalid number of arguments in expression"))
    end
end

function parse( expr::Any )
  throw( LispError("Invalid type $expr") )
end

#
# Define CALC ==================================================
#

function calc( ast::Num )
    return ast.n
end

#For binary operations
function calc( ast::Binop )
    l = calc(ast.lhs)
    r = calc(ast.rhs)
    if ( ast.op == / ) && ( r == 0)
        throw( LispError("Division by 0 not permitted"))
    else
        return ast.op( l, r )
    end
end

#for Unary operations
function calc( ast::UnaryOp )
    number = calc(ast.operand)
    if(ast.op == collatz && number <= 0)
        throw( LispError("Cannot do collatz on $number") )
    else
        return ast.op( calc( ast.operand ))
    end
end

#
# Define Collatz ==================================================
#

function collatz( n::Real )
  return collatz_helper( n, 0 )
end

function collatz_helper( n::Real, num_iters::Int )
  if n == 1
    return num_iters
  end
  if mod(n,2)==0
    return collatz_helper( n/2, num_iters+1 )
  else
    return collatz_helper( 3*n+1, num_iters+1 )
  end
end

#
# ==================================================
#

function interp( cs::AbstractString )
    lxd = Lexer.lex( cs )
    ast = parse( lxd )
    return calc( ast )
end

# Define Dictionary ====================================
UnaryDictionary = Dict(:- => -, :collatz => collatz)
BinDictionary = Dict(:+ => +, :- => -, :* => *, :/ => /, :mod => mod, :collatz => collatz)
end #module
