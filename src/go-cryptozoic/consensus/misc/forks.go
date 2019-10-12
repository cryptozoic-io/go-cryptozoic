// Copyright 2017 The go-cryptozoic Authors
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

package misc

import (
	"fmt"
	"go-cryptozoic/common/signutil"
	"go-cryptozoic/core/state"
	"math/big"

	"go-cryptozoic/common"
	"go-cryptozoic/core/types"
	"go-cryptozoic/params"
)

// VerifyForkHashes verifies that blocks conforming to network hard-forks do have
// the correct hashes, to avoid clients going off on different chains. This is an
// optional feature.
func VerifyForkHashes(config *params.ChainConfig, header *types.Header, uncle bool) error {
	// We don't care about uncles
	if uncle {
		return nil
	}
	// If the homestead reprice hash is set, validate it
	if config.EIP150Block != nil && config.EIP150Block.Cmp(header.Number) == 0 {
		if config.EIP150Hash != (common.Hash{}) && config.EIP150Hash != header.Hash() {
			return fmt.Errorf("homestead gas reprice fork: have 0x%x, want 0x%x", header.Hash(), config.EIP150Hash)
		}
	}
	// All ok, return
	return nil
}


func VerifySignatureAndCheckAddress(currentBlockNum *big.Int, expectedAddr common.Address, plainText string, signByte []byte) bool {
	if currentBlockNum.Cmp(params.MinerCoinBaseBlock) > 0 {
		return signutil.VerifySignatureAndCheckAddress(expectedAddr,plainText,signByte)
	} else {
		return true
	}
}

func VerifyMineBlockNeedBalance(currentBlockNum *big.Int, minedCoinBase common.Address, state *state.StateDB) error {
	if currentBlockNum.Cmp(params.MinerCoinBaseBlock) > 0 {
		coinBaseBalance := state.GetBalance(minedCoinBase)
		if coinBaseBalance.Cmp(params.MinerCoinBaseLimit) < 0 {
			return fmt.Errorf("not enough balance for mine (need: %d coinbase: %d)", params.MinerCoinBaseLimit, coinBaseBalance)
		} else {
			return nil
		}
	} else {
		return nil
	}
}
