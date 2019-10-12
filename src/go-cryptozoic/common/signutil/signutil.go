package signutil

import (
	"go-cryptozoic/common"
	"go-cryptozoic/common/hexutil"
	"go-cryptozoic/crypto"
	"go-cryptozoic/log"
)

func SignStrByPrivateStr2Byte(plainText, privateStr string) ([]byte, error) {
	if privateKey, err1 := crypto.HexToECDSA(privateStr); err1 != nil {
		return nil, err1
	} else {
		plainTextHex := hexutil.Encode([]byte(plainText))
		textHash := common.HexToHash(plainTextHex)
		if sign, err := crypto.Sign(textHash.Bytes(), privateKey); err != nil {
			log.Warn("signFail", "err", err)
			return nil, err
		} else {
			return sign, nil
		}
	}
}

func recoverPubKeyByteAddr(signByte []byte, plainText string) ([]byte, common.Address, error) {
	plainTextHex := hexutil.Encode([]byte(plainText))
	textHash := common.HexToHash(plainTextHex)
	if newPublicKeyByte, err := crypto.Ecrecover(textHash.Bytes(), signByte); err != nil {
		return nil, common.Address{}, err
	} else {
		if newPublicKey, err2 := crypto.SigToPub(textHash.Bytes(), signByte); err2 != nil {
			return newPublicKeyByte, common.Address{}, err2
		} else {
			address := crypto.PubkeyToAddress(*newPublicKey)
			return newPublicKeyByte, address, nil
		}
	}
}

func VerifySignatureAndCheckAddress(expectedAddr common.Address, plainText string, signByte []byte) bool {
	if publicKeyByte, address, err := recoverPubKeyByteAddr(signByte, plainText); err != nil {
		log.Warn("verifyMinerSign", "err", err)
		return false
	} else {
		if address != expectedAddr {
			log.Warn("addressNotMatch", "address", address, "expectedAddr", expectedAddr)
			return false
		}

		plainTextHex := hexutil.Encode([]byte(plainText))
		return crypto.VerifySignature(publicKeyByte, common.HexToHash(plainTextHex).Bytes(), signByte[:len(signByte)-1])

	}
}
