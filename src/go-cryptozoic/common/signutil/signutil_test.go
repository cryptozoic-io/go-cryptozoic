package signutil

import (
	"fmt"
	"go-cryptozoic/common"
	"testing"
)

func TestSignAndVerify(t *testing.T) {

	privateKeyStr := ""
	plainText := "hello gvc!"
	//sign
	signByte, _ := SignStrByPrivateStr2Byte(plainText, privateKeyStr)

	//check

	verifyed := VerifySignatureAndCheckAddress(common.HexToAddress("0x6311EFb1E5D42eAf092C7b8Fb6663166fc98adbb"), plainText, signByte)
	fmt.Println(verifyed)
}
