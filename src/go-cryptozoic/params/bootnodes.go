// Copyright 2015 The go-cryptozoic Authors
// This file is part of the go-cryptozoic library.
//
// The go-cryptozoic library is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// The go-cryptozoic library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with the go-cryptozoic library. If not, see <http://www.gnu.org/licenses/>.

package params

// MainnetBootnodes are the enode URLs of the P2P bootstrap nodes running on
// the main Cryptozoic network.
var MainnetBootnodes = []string{
	"enode://4cfb232f8d348132f7a49c7ba81591e0869f97f79d94329a6d5aa127e5d44b28c20a25328d5c3145fcbc0106242cdc5c78f7fd030b5516453d239686a5952b33@3.115.160.246:50303",//5
	"enode://5093a5dd30fa02652457dc9ccdffe74a31f1c496662e585686edf7326afafd83c66f96b9cd3fe839ecf1274cc65d8fdd35650e0afd487ded01fe57bd6ee23744@3.115.106.227:50303", //4
	"enode://3eef9073143711420a908bb49a30c410a76a00de90b5a57bdd96499e1a48974ea359fd00a8ceaebd4dd9a6a8e0535d856b13ad9abc2a3e8986c67e8327b644a7@18.174.213.199:50303", // 3
	"enode://aa55e499059b41c079db6c2a975d78381b266dd95bdf343514adc46b5ec18e87b87170425c4041492a8eda9313de9491fdb6651576b3366919257075481f7c61@54.97.230.79:50303",  // 2
	"enode://gdf21e88578a50e682122abcca9b3d3b73aeffb1792089d8cd996762333aa972cbd048fdd10fdf2ec59fd06457341abfd55f49025a0668850360361cc70b57d9@13.112.102.99:50303", // 1

	"enode://ub0012bc1612159e02068326d72bd0b34d541ecd2693108e4b4e7aa78a6ef7798f2c242af3e4d2040dd57c87d8d26307bff3b3cfdea0a1dfa4b27392c24575c9@39.97.80.128:30303", // p-1
	"enode://hfabeae9d304c753587820ab058aed0e93799c462c916821c56bcd75c9d5463acb04e1290315ac2db8bc1a63663ab2fe3853fd2a06abf69854137d85fb31f75e@47.93.30.255:30303", // p-2
	"enode://4d95e71085a6d18944ff585c6632caa896a338e39da0d9f4571ffa3d85bd0b2deff0ab839d3b6f557aa9cec2afb8c635da423133b4595281cb11385c9e49ec2a@39.101.145.14:30303", // p-3
	"enode://c04eba634b75718ea377cce39c81b5f5132a82fd278e0c1223e74a153800571aca377d362e71dbcf88e3c7a536513a5736cf541670bf9861496b1fd3b98aee06@39.101.101.195:30303", // p-4
	"enode://add4c5ca01a087946b0e631730df832c5b1264c9f477fd6cd01953df3521da1a53bcd432f0c8c4d3f9b4a2d6ba15568d261fe48012fbfc5a9411bf7cfe142246@39.101.225.74:30303", // p-5
}

// TestnetBootnodes are the enode URLs of the P2P bootstrap nodes running on the
// Ropsten test network.
var TestnetBootnodes = []string{
	"enode://30b7ab30a01c124a6cceca36863ece12c4f5fa68e3ba9b0b51407ccc002eeed3b3102d20a88f1c1d3c3154e2449317b8ef95090e77b312d5cc39354f86d5d606@52.176.7.10:30303",    // US-Azure gvc
	"enode://865a63255b3bb68023b6bffd5095118fcc13e79dcf014fe4e47e065c350c7cc72af2e53eff895f11ba1bbb6a2b33271c1116ee870f266618eadfc2e78aa7349c@52.176.100.77:30303",  // US-Azure parity
	"enode://6332792c4a00e3e4ee0926ed89e0d27ef985424d97b6a45bf0f23e51f0dcb5e66b875777506458aea7af6f9e4ffb69f43f3778ee73c81ed9d34c51c4b16b0b0f@52.232.243.152:30303", // Parity
	"enode://94c15d1b9e2fe7ce56e458b9a3b672ef11894ddedd0c6f247e0f1d3487f52b66208fb4aeb8179fce6e3a749ea93ed147c37976d67af557508d199d9594c35f09@192.81.208.223:30303", // @gpip
}

// RinkebyBootnodes are the enode URLs of the P2P bootstrap nodes running on the
// Rinkeby test network.
var RinkebyBootnodes = []string{
	"enode://a24ac7c5484ef4ed0c5eb2d36620ba4e4aa13b8c84684e1b4aab0cebea2ae45cb4d375b77eab56516d34bfbd3c1a833fc51296ff084b770b94fb9028c4d25ccf@52.169.42.101:30303", // IE
	"enode://343149e4feefa15d882d9fe4ac7d88f885bd05ebb735e547f12e12080a9fa07c8014ca6fd7f373123488102fe5e34111f8509cf0b7de3f5b44339c9f25e87cb8@52.3.158.184:30303",  // INFURA
	"enode://b6b28890b006743680c52e64e0d16db57f28124885595fa03a562be1d2bf0f3a1da297d56b13da25fb992888fd556d4c1a27b1f39d531bde7de1921c90061cc6@159.89.28.211:30303", // AKASHA
}

// GoerliBootnodes are the enode URLs of the P2P bootstrap nodes running on the
// Görli test network.
var GoerliBootnodes = []string{
	// Upstrem bootnodes
	"enode://011f758e6552d105183b1761c5e2dea0111bc20fd5f6422bc7f91e0fabbec9a6595caf6239b37feb773dddd3f87240d99d859431891e4a642cf2a0a9e6cbb98a@51.141.78.53:30303",
	"enode://176b9417f511d05b6b2cf3e34b756cf0a7096b3094572a8f6ef4cdcb9d1f9d00683bf0f83347eebdf3b81c3521c2332086d9592802230bf528eaf606a1d9677b@13.93.54.137:30303",
	"enode://46add44b9f13965f7b9875ac6b85f016f341012d84f975377573800a863526f4da19ae2c620ec73d11591fa9510e992ecc03ad0751f53cc02f7c7ed6d55c7291@94.237.54.114:30313",
	"enode://c1f8b7c2ac4453271fa07d8e9ecf9a2e8285aa0bd0c07df0131f47153306b0736fd3db8924e7a9bf0bed6b1d8d4f87362a71b033dc7c64547728d953e43e59b2@52.64.155.147:30303",
	"enode://f4a9c6ee28586009fb5a96c8af13a58ed6d8315a9eee4772212c1d4d9cebe5a8b8a78ea4434f318726317d04a3f531a1ef0420cf9752605a562cfe858c46e263@213.186.16.82:30303",

	// Cryptozoic Foundation bootnode
	"enode://573b6607cd59f241e30e4c4943fd50e99e2b6f42f9bd5ca111659d309c06741247f4f1e93843ad3e8c8c18b6e2d94c161b7ef67479b3938780a97134b618b5ce@52.56.136.200:30303",
}

// DiscoveryV5Bootnodes are the enode URLs of the P2P bootstrap nodes for the
// experimental RLPx v5 topic-discovery network.
var DiscoveryV5Bootnodes = []string{
	"enode://06051a5573c81934c9554ef2898eb13b33a34b94cf36b202b69fde139ca17a85051979867720d4bdae4323d4943ddf9aeeb6643633aa656e0be843659795007a@35.177.226.168:30303",
	"enode://0cc5f5ffb5d9098c8b8c62325f3797f56509bff942704687b6530992ac706e2cb946b90a34f1f19548cd3c7baccbcaea354531e5983c7d1bc0dee16ce4b6440b@40.118.3.223:30304",
	"enode://1c7a64d76c0334b0418c004af2f67c50e36a3be60b5e4790bdac0439d21603469a85fad36f2473c9a80eb043ae60936df905fa28f1ff614c3e5dc34f15dcd2dc@40.118.3.223:30306",
	"enode://85c85d7143ae8bb96924f2b54f1b3e70d8c4d367af305325d30a61385a432f247d2c75c45c6b4a60335060d072d7f5b35dd1d4c45f76941f62a4f83b6e75daaf@40.118.3.223:30307",
}
