package config

import (
	"errors"
	"fmt"
	"strings"

	"github.com/jmattheis/goverter/enum"
)

type Common struct {
	FieldSettings                      []string
	WrapErrors                         bool
	WrapErrorsUsing                    string
	IgnoreUnexported                   bool
	MatchIgnoreCase                    bool
	IgnoreMissing                      bool
	SkipCopySameType                   bool
	UseZeroValueOnPointerInconsistency bool
	UseUnderlyingTypeMethods           bool
	Enum                               enum.Config

	// SearchConfig configures the source-to-target struct-fields mapper to
	// map fields (i.e., source and target struct fields)
	// using SearchConfig.SearchMode
	SearchConfig SearchConfig
}

// SearchConfig value to use in mapping source to target.
// Note that tag keys are case-sensitive
type SearchConfig struct {

	// SourceTagKey specifies the tag key to lookout for in source field's tag.
	// SourceTagKey can not be empty if SearchMode is any of
	// SearchModeTagToTag or SearchModeTagToFieldName
	SourceTagKey string

	// TargetTagKey specifies the tag key to lookout for in a target field's tag.
	// TargetTagKey can not be empty if SearchMode is any of
	// SearchModeTagToTag or SearchModeFieldNameToTag
	TargetTagKey string
	SearchMode   SearchMode
}

type SearchMode int

const (
	// SearchModeFieldNameToFieldName is the default search mode.
	// Configures mapper to find corresponding field by comparing
	// sourceFieldNames to targetFieldNames
	SearchModeFieldNameToFieldName SearchMode = iota

	// SearchModeTagToTag configures mapper to find corresponding field
	// by comparing sourceField tagValue to targetField tagValue
	SearchModeTagToTag

	// SearchModeTagToFieldName configures mapper to find corresponding field
	// by comparing sourceField tagValue to target FieldName
	SearchModeTagToFieldName

	// SearchModeFieldNameToTag configures mapper to find corresponding field
	// by comparing source FieldName to targetField tagValue
	SearchModeFieldNameToTag
)

func NewSearchConfig(sourceConfig, targetConfig string) SearchConfig {
	if useFieldName(sourceConfig) && useFieldName(targetConfig) {
		return SearchConfig{
			SearchMode: SearchModeFieldNameToFieldName,
		}
	}

	if sourceTagKey := extractTagKey(sourceConfig); sourceTagKey != "" {
		if useFieldName(targetConfig) {
			return SearchConfig{
				SearchMode:   SearchModeTagToFieldName,
				SourceTagKey: sourceTagKey,
			}
		}

		if targetTagKey := extractTagKey(targetConfig); targetTagKey != "" {
			return SearchConfig{
				SearchMode:   SearchModeTagToTag,
				SourceTagKey: sourceTagKey,
				TargetTagKey: targetTagKey,
			}
		}

		panic(errors.New("can not determine search mode"))
	}

	if targetTagKey := extractTagKey(targetConfig); targetTagKey != "" {
		if useFieldName(sourceConfig) {
			return SearchConfig{
				SearchMode:   SearchModeFieldNameToTag,
				TargetTagKey: targetTagKey,
			}
		}
	}

	panic(errors.New("can not determine search mode"))
}

func useFieldName(s string) bool {
	return strings.EqualFold(s, "fieldName")
}

// extractTagKey from s, where s is expected to be formatted like so tag:key
func extractTagKey(s string) (tagKey string) {
	parts := strings.Split(s, ":")
	if len(parts) != 2 {
		return ""
	}
	return parts[1]
}

func parseCommon(c *Common, cmd, rest string) (fieldSetting bool, err error) {
	switch cmd {
	case "wrapErrors":
		if c.WrapErrorsUsing != "" {
			return false, fmt.Errorf("cannot be used in combination with wrapErrorsUsing")
		}
		c.WrapErrors, err = parseBool(rest)
	case "wrapErrorsUsing":
		if c.WrapErrors {
			return false, fmt.Errorf("cannot be used in combination with wrapErrors")
		}
		c.WrapErrorsUsing, err = parseString(rest)
	case "searchMode":
		parts := strings.Fields(rest)
		if len(parts) != 2 {
			err = errors.New(`searchMode must be formatted like so
sourceWhere targetWhere.
For example
tag:json useFieldName
useFieldName tag:bson
tag:xml tag:xml
`)
		}
		c.SearchConfig = NewSearchConfig(parts[0], parts[1])
	case "ignoreUnexported":
		fieldSetting = true
		c.IgnoreUnexported, err = parseBool(rest)
	case "matchIgnoreCase":
		fieldSetting = true
		c.MatchIgnoreCase, err = parseBool(rest)
	case "ignoreMissing":
		fieldSetting = true
		c.IgnoreMissing, err = parseBool(rest)
	case "skipCopySameType":
		c.SkipCopySameType, err = parseBool(rest)
	case "useZeroValueOnPointerInconsistency":
		c.UseZeroValueOnPointerInconsistency, err = parseBool(rest)
	case "useUnderlyingTypeMethods":
		c.UseUnderlyingTypeMethods, err = parseBool(rest)
	case "enum":
		c.Enum.Enabled, err = parseBool(rest)
	case "enum:unknown":
		c.Enum.Unknown, err = parseString(rest)
		if err == nil && IsEnumAction(c.Enum.Unknown) {
			err = validateEnumAction(c.Enum.Unknown)
		}
	case "":
		err = fmt.Errorf("missing setting key")
	default:
		err = fmt.Errorf("unknown setting: %s", cmd)
	}

	return fieldSetting, err
}
