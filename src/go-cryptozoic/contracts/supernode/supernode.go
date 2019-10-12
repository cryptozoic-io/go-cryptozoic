package supernode

import (
	"go-cryptozoic/accounts/abi"
	"strings"
	"go-cryptozoic/ethclient"
	"go-cryptozoic/params"
	"go-cryptozoic/core/types"
	"go-cryptozoic/common"
	"go-cryptozoic/accounts/abi/bind"
	"math/big"
	"go-cryptozoic/log"
	"fmt"
)

var(
	abiJSON="[{\"constant\":true,\"inputs\":[],\"name\":\"queryAllSuperNodeAddreses\",\"outputs\":[{\"name\":\"\",\"type\":\"address[]\"}],\"payable\":false,\"stateMutability\":\"view\",\"type\":\"function\"},{\"constant\":true,\"inputs\":[],\"name\":\"superNodeNeedLockAmount\",\"outputs\":[{\"name\":\"\",\"type\":\"uint256\"}],\"payable\":false,\"stateMutability\":\"view\",\"type\":\"function\"},{\"constant\":false,\"inputs\":[{\"name\":\"_superNodeAddr\",\"type\":\"address\"}],\"name\":\"refreshSuperNodeActiveBlock\",\"outputs\":[],\"payable\":false,\"stateMutability\":\"nonpayable\",\"type\":\"function\"},{\"constant\":false,\"inputs\":[],\"name\":\"withdraw\",\"outputs\":[],\"payable\":false,\"stateMutability\":\"nonpayable\",\"type\":\"function\"},{\"constant\":false,\"inputs\":[],\"name\":\"renounceOwnership\",\"outputs\":[],\"payable\":false,\"stateMutability\":\"nonpayable\",\"type\":\"function\"},{\"constant\":true,\"inputs\":[],\"name\":\"superNodeCount\",\"outputs\":[{\"name\":\"\",\"type\":\"uint256\"}],\"payable\":false,\"stateMutability\":\"view\",\"type\":\"function\"},{\"constant\":true,\"inputs\":[],\"name\":\"owner\",\"outputs\":[{\"name\":\"\",\"type\":\"address\"}],\"payable\":false,\"stateMutability\":\"view\",\"type\":\"function\"},{\"constant\":true,\"inputs\":[],\"name\":\"isOwner\",\"outputs\":[{\"name\":\"\",\"type\":\"bool\"}],\"payable\":false,\"stateMutability\":\"view\",\"type\":\"function\"},{\"constant\":true,\"inputs\":[{\"name\":\"_nodeAddress\",\"type\":\"address\"}],\"name\":\"judgeSuperNode\",\"outputs\":[{\"name\":\"\",\"type\":\"bool\"}],\"payable\":false,\"stateMutability\":\"view\",\"type\":\"function\"},{\"constant\":true,\"inputs\":[{\"name\":\"\",\"type\":\"address\"}],\"name\":\"superNodeInfo\",\"outputs\":[{\"name\":\"addr\",\"type\":\"address\"},{\"name\":\"activeBlockNum\",\"type\":\"uint256\"},{\"name\":\"activeAmount\",\"type\":\"uint256\"},{\"name\":\"nodeArrayIndex\",\"type\":\"uint256\"}],\"payable\":false,\"stateMutability\":\"view\",\"type\":\"function\"},{\"constant\":true,\"inputs\":[{\"name\":\"\",\"type\":\"address\"}],\"name\":\"superNodeFlag\",\"outputs\":[{\"name\":\"\",\"type\":\"uint256\"}],\"payable\":false,\"stateMutability\":\"view\",\"type\":\"function\"},{\"constant\":true,\"inputs\":[{\"name\":\"\",\"type\":\"uint256\"}],\"name\":\"superNodeArray\",\"outputs\":[{\"name\":\"\",\"type\":\"address\"}],\"payable\":false,\"stateMutability\":\"view\",\"type\":\"function\"},{\"constant\":false,\"inputs\":[{\"name\":\"_superNodeNeedLockAmount\",\"type\":\"uint256\"}],\"name\":\"refreshSuperNodeNeedLockAmount\",\"outputs\":[],\"payable\":false,\"stateMutability\":\"nonpayable\",\"type\":\"function\"},{\"constant\":true,\"inputs\":[],\"name\":\"superNodeLockAmount\",\"outputs\":[{\"name\":\"\",\"type\":\"uint256\"}],\"payable\":false,\"stateMutability\":\"view\",\"type\":\"function\"},{\"constant\":true,\"inputs\":[{\"name\":\"_needCount\",\"type\":\"uint256\"}],\"name\":\"queryNeedCountSuperNodeAddreses\",\"outputs\":[{\"name\":\"\",\"type\":\"address[]\"}],\"payable\":false,\"stateMutability\":\"view\",\"type\":\"function\"},{\"constant\":false,\"inputs\":[{\"name\":\"newOwner\",\"type\":\"address\"}],\"name\":\"transferOwnership\",\"outputs\":[],\"payable\":false,\"stateMutability\":\"nonpayable\",\"type\":\"function\"},{\"constant\":true,\"inputs\":[{\"name\":\"_nodeAddress\",\"type\":\"address\"}],\"name\":\"queryNodeInfo\",\"outputs\":[{\"name\":\"addr\",\"type\":\"address\"},{\"name\":\"activeBlockNum\",\"type\":\"uint256\"},{\"name\":\"activeAmount\",\"type\":\"uint256\"}],\"payable\":false,\"stateMutability\":\"view\",\"type\":\"function\"},{\"inputs\":[],\"payable\":true,\"stateMutability\":\"payable\",\"type\":\"constructor\"},{\"payable\":true,\"stateMutability\":\"payable\",\"type\":\"fallback\"},{\"anonymous\":false,\"inputs\":[{\"indexed\":true,\"name\":\"previousOwner\",\"type\":\"address\"},{\"indexed\":true,\"name\":\"newOwner\",\"type\":\"address\"}],\"name\":\"OwnershipTransferred\",\"type\":\"event\"}]"
	parsedAbi, _ = abi.JSON(strings.NewReader(abiJSON))
	Caller *ethclient.Client
)
type NodeInfo struct {
	Addr common.Address
	ActiveBlockNum *big.Int
	ActiveAmount *big.Int
}

func QuerySuperNodes(config *params.ChainConfig,header *types.Header) []common.Address{
	var superNodes []common.Address
	filteredSuperNodes:=[]common.Address{}

	log.Info("connecting to superNode contract ","caller",Caller)

	superPoolContract :=bind.NewBoundContract(config.SuperNodeContractAddress,parsedAbi,Caller,nil,nil)
	//
	callopts:=&bind.CallOpts{Pending:false,BlockNumber:new(big.Int).Sub(header.Number,big.NewInt(1))};
	//
	superPoolContract.Call(callopts,&superNodes,"queryNeedCountSuperNodeAddreses",CalacNeedSuperNodeNumByCurrentBlock(header.Number))

	log.Info("node","nodesCount",len(superNodes))
	if superNodes!=nil&&len(superNodes)>0{
		for _,node:=range superNodes{
			nodeInfo :=&NodeInfo{
				Addr:common.HexToAddress("0x0000000000000000000000000000000000000000"),
				ActiveBlockNum: big.NewInt(0),
				ActiveAmount: big.NewInt(0),
			}
			//nodeInfoMap:=make(map[string]interface{})
			if err:=superPoolContract.Call(callopts,nodeInfo,"queryNodeInfo",node);err!=nil{
				fmt.Println("call contract fail",err)
			}

			//log.Info("nodeInfo","nodesAddress",node,"nodeInfoAddress",nodeInfo.Addr," nodeActiveBlockNum",nodeInfo.ActiveBlockNum,"nodeAmount",nodeInfo.ActiveAmount)


			if nodeInfo.ActiveBlockNum.Cmp(big.NewInt(0))>0{
				activeHeight:=new(big.Int).Sub(header.Number,nodeInfo.ActiveBlockNum)
				if activeHeight.Cmp(config.SuperNodeLockHeigth)>=0 {
					filteredSuperNodes = append(filteredSuperNodes,node)
				}
			}

		}
	}
	log.Info("superNodeCount","superNodeCount",len(superNodes),"canRewardCount",len(filteredSuperNodes))
	return filteredSuperNodes
}

