package supernode

import (
	"go-cryptozoic/period"
	"math/big"
)

var (
	superNodeAddNumStartBlock = big.NewInt(554240)
	initSuperNodeNum          = big.NewInt(128)
	firstPeriodEndBlock       = big.NewInt(1280000)
	secondPeriodEndBlock      = big.NewInt(3840000)
	firstPeriodAddNum         = big.NewInt(12)
	secondPeriodAddNum        = big.NewInt(6)
	FirstPeriodInitNum        = initSuperNodeNum

	SecondPeriodInitNum = doCalacNeedSuperNodeNum(superNodeAddNumStartBlock, firstPeriodEndBlock, FirstPeriodInitNum, firstPeriodAddNum)

	ThirdPeriodInitNum = doCalacNeedSuperNodeNum(firstPeriodEndBlock, secondPeriodEndBlock, SecondPeriodInitNum, secondPeriodAddNum)
)

func CalacNeedSuperNodeNumByCurrentBlock(currentBlock *big.Int) *big.Int {

	if currentBlock.Cmp(secondPeriodEndBlock) > 0 {
		return ThirdPeriodInitNum
	}

	if currentBlock.Cmp(firstPeriodEndBlock) > 0 {
		return doCalacNeedSuperNodeNum(firstPeriodEndBlock, currentBlock, SecondPeriodInitNum, secondPeriodAddNum);
	}

	if currentBlock.Cmp(superNodeAddNumStartBlock) > 0 {
		return doCalacNeedSuperNodeNum(superNodeAddNumStartBlock, currentBlock, initSuperNodeNum, firstPeriodAddNum);
	}

	return initSuperNodeNum
}
func doCalacNeedSuperNodeNum(baseblock, currentBlock, initNum, addNum *big.Int) *big.Int {

	week := period.CalacWeekAfterBlock(baseblock, currentBlock)
	totalAddNum := new(big.Int).Mul(week, addNum)
	return new(big.Int).Add(initNum, totalAddNum)
}
