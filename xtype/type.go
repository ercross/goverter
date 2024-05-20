package xtype

import (
	"go/types"
	"strings"

	"github.com/dave/jennifer/jen"
)

// ThisVar is used as name for the reference to the converter interface.
const ThisVar = "c"

// Signature represents a signature for conversion.
type Signature struct {
	Source string
	Target string
}

func SignatureOf(source, target *Type) Signature {
	return Signature{Source: source.String, Target: target.String}
}

// Type is a helper wrapper for types.Type.
type Type struct {
	String        string
	T             types.Type
	Interface     bool
	InterfaceType *types.Interface
	Struct        bool
	StructType    *types.Struct
	Named         bool
	NamedType     *types.Named
	Pointer       bool
	PointerType   *types.Pointer
	PointerInner  *Type
	List          bool
	ListFixed     bool
	ListInner     *Type
	Map           bool
	MapType       *types.Map
	MapKey        *Type
	MapValue      *Type
	Basic         bool
	BasicType     *types.Basic
	Signature     bool
	SignatureType *types.Signature
	Func          bool
	FuncType      *types.Func

	enum *Enum
}

func (t *Type) AssignableTo(other *Type) bool {
	return types.AssignableTo(t.T, other.T)
}

func (t *Type) AsPointer() *Type {
	return TypeOf(t.AsPointerType())
}

func (t *Type) AsPointerType() *types.Pointer {
	return types.NewPointer(t.T)
}

func (t *Type) InStruct(source *Type, field string) *Type {
	if t.Signature && source.Named {
		t.FuncType = types.NewFunc(-1, source.NamedType.Obj().Pkg(), field, t.SignatureType)
		t.Func = true
	}

	return t
}

// JenID a jennifer code wrapper with extra infos.
type JenID struct {
	ParentPointer *JenID
	Code          *jen.Statement
	Variable      bool
}

func (j *JenID) Pointer(t *Type, namer func(string) string) ([]jen.Code, *JenID) {
	if j.Variable {
		return nil, OtherID(jen.Op("&").Add(j.Code.Clone()))
	}

	name := namer(t.ID())
	stmt := []jen.Code{jen.Id(name).Op(":=").Add(j.Code.Clone())}
	return stmt, OtherID(jen.Op("&").Id(name))
}

// VariableID is used, when the ID can be referenced. F.ex it is not a function call.
func VariableID(code *jen.Statement) *JenID {
	return &JenID{Code: code, Variable: true}
}

// OtherID is used, when the ID isn't a variable id.
func OtherID(code *jen.Statement) *JenID {
	return &JenID{Code: code, Variable: false}
}

// TypeOf creates a Type.
func TypeOf(t types.Type) *Type {
	t = Unalias(t)
	rt := &Type{}
	rt.T = t
	rt.String = t.String()
	applyTo(rt, t)
	return rt
}

func applyTo(rt *Type, t types.Type) {
	switch value := t.(type) {
	case *types.Pointer:
		rt.Pointer = true
		rt.PointerType = value
		rt.PointerInner = TypeOf(value.Elem())
	case *types.Basic:
		rt.Basic = true
		rt.BasicType = value
	case *types.Map:
		rt.Map = true
		rt.MapType = value
		rt.MapKey = TypeOf(value.Key())
		rt.MapValue = TypeOf(value.Elem())
	case *types.Slice:
		rt.List = true
		rt.ListInner = TypeOf(value.Elem())
	case *types.Array:
		rt.List = true
		rt.ListFixed = true
		rt.ListInner = TypeOf(value.Elem())
	case *types.Named:
		rt.Named = true
		rt.NamedType = value
		applyTo(rt, value.Underlying())
	case *types.Struct:
		rt.Struct = true
		rt.StructType = value
	case *types.Interface:
		rt.Interface = true
		rt.InterfaceType = value
	case *types.Signature:
		rt.Signature = true
		rt.SignatureType = value
	case *types.TypeParam:
		// ignore
	default:
		panic("unknown types.Type " + t.String())
	}
}

// ID returns a deteministically generated id that may be used as variable.
func (t *Type) ID() string {
	return t.asID(true, true)
}

// UnescapedID returns a deteministically generated id that may be used as variable
// reserved keywords aren't escaped.
func (t *Type) UnescapedID() string {
	return t.asID(true, false)
}

func (t *Type) asID(seeNamed, escapeReserved bool) string {
	if seeNamed && t.Named {
		pkg := t.NamedType.Obj().Pkg()
		name := t.NamedType.Obj().Name()
		switch {
		case pkg != nil:
			name = pkg.Name() + name
		case escapeReserved:
			name = "x" + name
		}
		return name
	}
	if t.List {
		return t.ListInner.asID(true, false) + "List"
	}
	if t.Basic {
		if escapeReserved {
			return "x" + t.BasicType.String()
		}
		return t.BasicType.String()
	}
	if t.Pointer {
		return "p" + strings.Title(t.PointerInner.asID(true, false))
	}
	if t.Map {
		return "map" + strings.Title(t.MapKey.asID(true, false)+strings.Title(t.MapValue.asID(true, false)))
	}
	if t.Struct {
		return "unnamed"
	}
	return "unknown"
}

// TypeAsJen returns a jen representation of the type.
func (t Type) TypeAsJen() *jen.Statement {
	if t.Named {
		return toCode(t.NamedType)
	}
	return toCode(t.T)
}
