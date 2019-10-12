package period

import "math/big"

var (
	weekPeriod = big.NewInt(60480)
)

func CalacWeekAfterBlock(baseBlockNum *big.Int, currentBlockNum *big.Int) *big.Int {
	if baseBlockNum.Cmp(currentBlockNum) > 0 {
		return big.NewInt(0)
	}
	differentBlockNum := new(big.Int).Sub(currentBlockNum, baseBlockNum)
	theWeek := new(big.Int).Div(differentBlockNum, weekPeriod)
	return new(big.Int).Add(theWeek, big.NewInt(1))
}
