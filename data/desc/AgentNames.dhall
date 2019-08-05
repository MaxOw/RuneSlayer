let types      = ./Types.dhall

let AgentTypeName = types.AgentTypeName
let makeName = AgentTypeName.MakeAgentTypeName

in
{ bat        = makeName "Bat"
, spider     = makeName "Spider"

, npcBertram = makeName "Old Man Bartram"
}
