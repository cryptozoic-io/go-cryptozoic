package supernode

import (
	"fmt"
	"math/big"
	"testing"
)

func TestNumBlock(t *testing.T) {
	fmt.Println(FirstPeriodInitNum)
	fmt.Println(SecondPeriodInitNum)
	fmt.Println(ThirdPeriodInitNum)
}

func TestCalac1AndCalac2(t *testing.T) {

	fmt.Println("###x1", CalacNeedSuperNodeNumByCurrentBlock(big.NewInt(554239)))

	fmt.Println("###a1", CalacNeedSuperNodeNumByCurrentBlock(big.NewInt(554240)))

	fmt.Println("###b1", CalacNeedSuperNodeNumByCurrentBlock(big.NewInt(554241)))

	fmt.Println("###c1", CalacNeedSuperNodeNumByCurrentBlock(big.NewInt(654241)))

	fmt.Println("###d1", CalacNeedSuperNodeNumByCurrentBlock(big.NewInt(1920000)))

	fmt.Println("###e1", CalacNeedSuperNodeNumByCurrentBlock(big.NewInt(1990000)))

	fmt.Println("###f1", CalacNeedSuperNodeNumByCurrentBlock(big.NewInt(3840000)))

	fmt.Println("###g1", CalacNeedSuperNodeNumByCurrentBlock(big.NewInt(4840000)))
}
