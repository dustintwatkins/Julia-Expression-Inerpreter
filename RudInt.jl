#
# Class Interpreter 0
# Base interpreter with numbers, plus, and minus
#

module RubInt

push!(LOAD_PATH, ".")

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

type UnaryOp :< AE
    op::Function
    next::AE
end

# Define Dictionary ====================================

function Dict(symbol)
    if symbol == :+
        return +
    elseif symbol == :-
        return -
    elseif symbol == :*
        return *
    elseif symbol == :/
        return /
    elseif symbol == :mod
        return mod
    elseif symbol == :collatz
        return collatz
    end
end

#
# ==================================================
#

function parse( expr::Number )
    return Num( expr )
end

function parse( expr::Array{Any} )

    if size(expr)[1] == 1
        try
            return parse(expr[1])
        catch
            throw( LispError("Incorrect Input"))
        end
    elseif size(expr)[1] == 2                        #UnaryOperator Expression
        if expr[1] == :-                            #Parse it
            next = parse(expr[2])
        elseif expr[1] == :collatz                  #Collatz
            next = parse(expr[2])
        else
            throw( LispError("Incorrect Input for Unary Operator"))
        end
        return UnaryOp( Dict(expr[1]), next)
    elseif size(expr)[1] == 3
        if expr[1] == :+
            lhs = parse(expr[2])
            rhs = parse(expr[3])
        

    end

    throw(LispError("Unknown operator!"))
end

function parse( expr::Any )
  throw( LispError("Invalid type $expr") )
end

#
# ==================================================
#

function calc( ast::Num )
    return ast.n
end

function calc( ast::Binop )
    return calc( ast.lhs ) + calc( ast.rhs )
end

function calc( ast::MinusNode )
    return calc( ast.lhs ) - calc( ast.rhs )
end

#
# ==================================================
#

function interp( cs::AbstractString )
    lxd = Lexer.lex( cs )
    ast = parse( lxd )
    return calc( ast )
end

end #module
