let types      = ./Types.dhall

let AgentTypeName = types.AgentTypeName
let makeName = AgentTypeName.MakeAgentTypeName

in
{ bat        = makeName "bat"
, spider     = makeName "spider"

, npcBertram = makeName "npcBartram"
}
