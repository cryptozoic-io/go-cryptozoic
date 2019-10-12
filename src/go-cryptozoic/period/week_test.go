package period

import (
	"fmt"
	"math/big"
	"testing"
)

func TestCalacWeekAfterBlock(t *testing.T) {
	fmt.Println(CalacWeekAfterBlock(big.NewInt(398490),big.NewInt(398491)))
}
