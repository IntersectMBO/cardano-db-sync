{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.DbSync.Util.AddressTest (tests) where

import Cardano.Binary (FromCBOR (..), unsafeDeserialize')
import Cardano.DbSync.Util.Address
import Cardano.Ledger.Address (Addr (..), BootstrapAddress (..), RewardAcnt (..))
import Cardano.Ledger.BaseTypes (Network (..))
import qualified Cardano.Ledger.Binary.Decoding as Decoding
import Cardano.Ledger.Crypto (StandardCrypto ())
import Cardano.Prelude
import Data.ByteString.Base16 (decodeLenient)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import Hedgehog.Gen.QuickCheck (arbitrary)
import Test.Cardano.Ledger.Core.Arbitrary ()
import Prelude ()

tests :: IO Bool
tests =
  checkParallel $
    Group
      "Cardano.DbSync.Util.Address"
      [ ("serialiseAddress byron simple", prop_serialiseAddress_byron)
      , ("serialiseAddress byron roundtrip", prop_serialiseAddress_byron_roundtrip)
      , ("serialiseAddress shelley simple", prop_serialiseAddress_shelley)
      , ("serialiseAddress shelley roundtrip", prop_serialiseAddress_shelley_roundtrip)
      , ("serialiseRewardAcnt simple", prop_serialiseRewardAcnt)
      , ("serialiseRewardAcnt roundtrip", prop_serialiseRewardAcnt_roundtrip)
      ]

prop_serialiseAddress_byron :: Property
prop_serialiseAddress_byron = property $ do
  (cborHex, expected) <- forAll $ Gen.element knownByronAddresses
  let addr = AddrBootstrap . BootstrapAddress . deserialiseBase16 $ cborHex

  serialiseAddress addr === expected

prop_serialiseAddress_byron_roundtrip :: Property
prop_serialiseAddress_byron_roundtrip = property $ do
  addr <- AddrBootstrap <$> forAll genByronAddress
  tripping addr serialiseAddress deserialiseByronAddress

prop_serialiseAddress_shelley :: Property
prop_serialiseAddress_shelley = property $ do
  (cborHex, expected) <- forAll $ Gen.element knownShelleyAddresses
  let addr = decodeBase16 cborHex

  serialiseAddress addr === expected

prop_serialiseAddress_shelley_roundtrip :: Property
prop_serialiseAddress_shelley_roundtrip = property $ do
  addr <- forAll genShelleyAddress
  cover 10 "mainnet" $ getNetwork addr == Mainnet
  cover 10 "testnet" $ getNetwork addr == Testnet

  tripping addr serialiseAddress deserialiseShelleyAddress

prop_serialiseRewardAcnt :: Property
prop_serialiseRewardAcnt = property $ do
  (cborHex, expected) <- forAll $ Gen.element knownStakeAddresses
  let addr = decodeBase16 cborHex

  serialiseRewardAcnt addr === expected

prop_serialiseRewardAcnt_roundtrip :: Property
prop_serialiseRewardAcnt_roundtrip = property $ do
  acnt <- forAll genRewardAcnt
  cover 10 "mainnet" $ getRwdNetwork acnt == Mainnet
  cover 10 "testnet" $ getRwdNetwork acnt == Testnet

  tripping acnt serialiseRewardAcnt deserialiseRewardAcnt

knownByronAddresses :: [(Text, Text)]
knownByronAddresses =
  [
    ( "82d818584583581c88c83c3233586b4d4b532c28138b14f6a846c1357084f10fb9f127f4a201581e581cc060ac356322c30490e6d3651e06966488c9107790f79637142be660024102001a266025e8"
    , "KjgoiXJS2coXLVuJtDXwygCbRigh3rjWFGxRKjNfPbBGZBkNrbbWkn8waRJjy18HWDwF7pucni4jjymEH3BQmVJF2HZvhMuQ1XaXEFa9cNwZ"
    )
  ,
    ( "82d818584583581cde01ec5a016bff1d9369bf00e32cc1706a7a9b9afc17a4255140bac3a201581e581cfb9930dad5f5114cb47851ef1fa8945b1c7f2d0a6d4ec7ff0652e382024102001ac617725a"
    , "KjgoiXJS2coosFbyBnUaTexmeK4mx3mRk93RvPhBzbZasB4Z6YtWQ52iQ1BBs3u5wLGaPjngMQMNpKU5U4X5fzzWrArPYWwwnjTB2rUK9Spq"
    )
  ,
    ( "82d818584583581cc5f5ef964d2a688c60788572954b079e914df4408b34bbca7875b504a201581e581caf2aeaa72ba78fffc1e467717359f505edafd7bce21a8d08c32531ff024102001a41121feb"
    , "KjgoiXJS2cojCjZuAm1SGgH95DdnczwyQbtxmdC2BLThdt9FLJsxrJbTYbGeNj4rj9Lt6PB8LGudabtEcTUvkN6tSTYiDxdm3MiiRnFpBqWS"
    )
  ,
    ( "82d818584583581ce82ca6a598d5f2d5087ee78471ec1f44e6650fa427d0572bb97be32da201581e581ced260a7012e6e27d385a8b3687efc9d51759051a5accdfeafd8fbbec024102001a7db597f5"
    , "KjgoiXJS2coqqdHnq2Dmxiscmwiujc8Nvg8q5E6JTr8sCJRkf4N6hZ7ombJfz2KJDdFm29N5DuETHvD7H9ZVgt1E6N5K92hnqP7SGcUQGmJ4"
    )
  ,
    ( "82d818584583581cbb40f5b9069ca7793cdd8b3909cc6d6a7a1a7e5cf81ff4dbfb6fd20ea201581e581c5d9565ff31488a1c4fd432c4e45ae361ceccc2681396ab92edb1ce7f024102001aabc726c6"
    , "KjgoiXJS2coh8HXDxJddhGrUqnJoJZgfhgKaQnDva5BCw5ujyTMsQsgi5PL6mAmDWdReh5eMWq8tF2GoLHbxuSjfWvy4CULXNt2tbuG5DZN5"
    )
  ,
    ( "82d818584583581c3d0f72aa88d845ff8280304f719544c74bd0a136d192a8b21ca6d406a201581e581cc864e3ca51f97b7b8a4da0eaf726a745b5d6f395192665f1186e4475024102001a2dafdf54"
    , "KjgoiXJS2coGef3TEVrpZDAcNd3QPcJd86dF2YEZAe8Y9pmQxZNa4W25ceVERE4JRafgEBFaX9Ejphfbbjc8JP9okF8cx6pynitvyrxjdCkj"
    )
  ,
    ( "82d818584583581c8b934975b369e9583302414ab63ad9f5aa25085fccf75d86eb3779c2a201581e581c1dd233ef7def1d2fa9202f547d25bf5f67d7f6015867c2d989035ab8024102001a2cc9a7e9"
    , "KjgoiXJS2coXsvPP38HXxV9Wk8NNnaGQuK3grYVXJQmVkoVo6PFoS1AmVnkF44vRXTCtuuMVDskm7is64ZmYNgfaana1Pj4pvUNFSSp3vCHv"
    )
  ,
    ( "82d818584583581cd9606595bd33394f4a7ef17b8ee163f2914aae2350a0e3a61d413c7aa201581e581c3fc3b189e8d868fa5f5228e0ac5c2f5b9de1ac046a65e1825a4212ac024102001a0088b9e2"
    , "KjgoiXJS2cony9zyXRBo8SwczFNmyrcKK6ABhjLVNWhnMoUC8FAzyerg6NJ7FVdWSYZJ7hEVxCHkhG8c6CMi1ptdp7YyymjAQL8iWBiWm1aR"
    )
  ,
    ( "82d818584583581c0c0df845d014fc4f2ec41079b123ecc100fe1e0eb1926707c12abc5ba201581e581ca0db087535692196d3c6526138996d1ff4c50f0b668fce3b48e376bf024102001ad5e5c937"
    , "KjgoiXJS2co79MqB9TqPUciV8DGaFWh7DV5Tenxmu5j9takY9NRQgvWt6zcSfZg8haReyXGmBpiJJ8mfKKZ4Bft14FniQruDVwpRLt21xhr6"
    )
  ,
    ( "82d818584583581c5e94e4ddbb2a4615615c5a8e36e02e0c54768dc9ee35665dbc28eb9ba201581e581ca68ea4a802ac7a54960e74dd5686df531ee18cf3bb4c20f25af62c24024102001ac60ad4dd"
    , "KjgoiXJS2coP9kxB7MWH7gtSZLqinKq849DBhPxxnduH2qtdZvab4MGHPrMBmA3fgXtqpt5Pi8FCcBxy9Tx4tMk5tJQXjrLexubPZc7z6QyS"
    )
  ,
    ( "82d818584583581c87b7faceb2c9add9e59d271b771d9bb3e9659b79df0d6e41ced6b36aa201581e581cb105d8c2825bed360f3ae73dde111f7c7638de3ee019aa9ebae3f80e024102001a569598e2"
    , "KjgoiXJS2coX8XzZkpJsCGPFiHSYgmgVx97aQAVNRZfyDfcotm3f4xCiR1PnpFBuC8rqm2b5mySypx6cuiCcZsvASwXNpi7P5ErY3RgTkR8u"
    )
  ]

knownShelleyAddresses :: [(Text, Text)]
knownShelleyAddresses =
  [
    ( "583900518e37aca4ae7f5f8319fc2e324b6f50584e9a74310360c30694ccddc5834d1b5b037bf626671ff235106944de11cee6b2840c29eea93403"
    , "addr_test1qpgcudav5jh87hurr87zuvjtdag9sn56wscsxcxrq62vehw9sdx3kkcr00mzvecl7g63q62ymcguae4jssxznm4fxspslx0h7a"
    )
  ,
    ( "58390023af6dd734bd77ef95eb3b0dddba1f9d29e94b7a5afe3b71215e27c91b85833f484c850cb42b140a6f43f6f6358ec18b37b4df6ee65090b4"
    , "addr_test1qq367mwhxj7h0mu4avasmhd6r7wjn62t0fd0uwm3y90z0jgmskpn7jzvs5xtg2c5pfh58ahkxk8vrzehkn0kaejsjz6qdjlhmu"
    )
  ,
    ( "583900c77b70d5239726dd3f25e16502f37d23495dbf7ea92b20388bef590d29c352a8499946388bc88d6c23de20e5f821d4ab61d2269b96b2294d"
    , "addr_test1qrrhkux4ywtjdhflyhsk2qhn0535jhdl065jkgpc30h4jrffcdf2sjvegcughjydds3aug89lqsaf2mp6gnfh94j99xs9dww6a"
    )
  ,
    ( "5839002c4ee31f5ee68283c83b9c382a1272cd7b16055ca06cdca0d39a40fd2ccaea528451e839ce1f9f15b6b04028e965719aed0dc2bb3c352eec"
    , "addr_test1qqkyaccltmng9q7g8wwrs2sjwtxhk9s9tjsxeh9q6wdyplfvet499pz3aquuu8ulzkmtqspga9jhrxhdphptk0p49mkq9sdgca"
    )
  ,
    ( "5839007805285a056ebe21004ed63fb720ba29438277629371b6c6b01a4429fb6632012d30e8148d9c9a958dcd38377d769bb04363bbbe86d999e3"
    , "addr_test1qpuq22z6q4htuggqfmtrldeqhg558qnhv2fhrdkxkqdyg20mvceqztfsaq2gm8y6jkxu6wph04mfhvzrvwamapken83saj5hp3"
    )
  ,
    ( "583900ddb3f8ba26ddca8309e51dcdb1122a2989e6c8bcf3c04ac896d5a120321e31d3f17bb8ceb2e73db6955b46d8bb202a1799e55a1bb87d122d"
    , "addr_test1qrwm8796ymwu4qcfu5wumvgj9g5cnekghneuqjkgjm26zgpjrcca8utmhr8t9eeak624k3kchvsz59ueu4dphwrazgkskl3l75"
    )
  ,
    ( "583900ebddd30e0e5b56f9b840c50e5907c0f5feda5c2c50718742533cdc0e983694746be4fe58c6cc0c4e6717a81019364fcb73679c31f00df2f9"
    , "addr_test1qr4am5cwped4d7dcgrzsukg8cr6lakju93g8rp6z2v7dcr5cx628g6lylevvdnqvfen302qsrymyljmnv7wrruqd7tusk8kpdw"
    )
  ,
    ( "5839009399c5ea582b81db09326adcf7e360a04583f2b25440391877eebe97d9c637e800a0b3c76f1027090d860cc44f28ca2f0bf1295f6f4392ce"
    , "addr_test1qzfen302tq4crkcfxf4dealrvzsytqljkf2yqwgcwlhta97eccm7sq9qk0rk7yp8pyxcvrxyfu5v5tct7y547m6rjt8qpglmd6"
    )
  ,
    ( "583900c2bbe53605cf272ff04f6ac4bb264c1a087170e98f0f02e34d6d7d603906202d06e72c8020e9030e33da8d3313ae7ae53d84e39253b2338f"
    , "addr_test1qrpthefkqh8jwtlsfa4vfwexfsdqsutsax8s7qhrf4kh6cpeqcsz6ph89jqzp6grpcea4rfnzwh84efasn3ey5ajxw8sp39t7t"
    )
  ,
    ( "5839001e76337d7c7e470599afb197eb53259829bc13d39b4f07fcf5e6017de3f0196e5cfba443ca9d1fc602af8ab84c57eb2d2c207a33739b6190"
    , "addr_test1qq08vvma03lywpve47ce066nykvzn0qn6wd57plu7hnqzl0r7qvkuh8m53pu48glccp2lz4cf3t7ktfvyparxuumvxgqeh840x"
    )
  ,
    ( "583900f41b07b5388bb834a3074d9d0202e19ad50eeccde0aa4307e2e070767a465b68ff8c5744155d645bb892197d63aa683c49dec0cb3b9bbc91"
    , "addr_test1qr6pkpa48z9msd9rqaxe6qszuxdd2rhvehs25sc8uts8qan6gedk3luv2azp2htytwufyxtavw4xs0zfmmqvkwumhjgsxmh6m3"
    )
  ]

knownStakeAddresses :: [(Text, Text)]
knownStakeAddresses =
  [
    ( "581de196459d8864d4edd4df94e092c8383249449a1fe9ff8d2c7171cbb702"
    , "stake1uxtyt8vgvn2wm4xljnsf9jpcxfy5fxsla8lc6tr3w89mwqs422x5g"
    )
  ,
    ( "581de17e9c9cb9010ba94b9f72b583002673ef38481adbdfa1b562719ff89d"
    , "stake1u9lfe89eqy96jjulw26cxqpxw0hnsjq6m006rdtzwx0l38gcvfv8f"
    )
  ,
    ( "581de1a5aed57c3977f20c8b6045601b3bcacb77cd3ba54f4ff4afdd510b71"
    , "stake1uxj6a4tu89mlyrytvpzkqxemet9h0nfm5485la90m4gskugzvgm9a"
    )
  ,
    ( "581de1b7940d3bcca7eff74bef0b651ddc61edcdb1ab48d15a489b87bb3e3a"
    , "stake1uxmegrfmejn7la6tau9k28wuv8kumvdtfrg45jyms7anuwsyq2q08"
    )
  ,
    ( "581de0bd3ba61077b2a09c23addccfc1853154f4f7736d268155420dbc3ba8"
    , "stake_test1uz7nhfssw7e2p8pr4hwvlsv9x920famnd5ngz42zpk7rh2q3rg0ut"
    )
  ,
    ( "581de0b5f7ef8b9d62519dea3b802546d5b07cb6de030d989cf82e899a71dd"
    , "stake_test1uz6l0mutn439r8028wqz23k4kp7tdhsrpkvfe7pw3xd8rhgepv79v"
    )
  ,
    ( "581de08048088222a5e99bde8e488fa0028ba6fc056bf974b14ac50f6cdf4d"
    , "stake_test1uzqyszyzy2j7nx773eyglgqz3wn0cpttl96tzjk9pakd7ngce60mf"
    )
  ,
    ( "581de0648ca5f1635a95d5eb9b9d0708a4c3ff18d19a587367b82d3c9b4016"
    , "stake_test1upjgef03vddft40tnwwswz9yc0l335v6tpek0wpd8jd5q9s93lzpw"
    )
  ,
    ( "581de09bcc41bb697bcc984f30731c648131e6c100d796cbf766977cca4cf9"
    , "stake_test1uzducsdmd9auexz0xpe3ceypx8nvzqxhjm9lwe5h0n9ye7g5x8jk7"
    )
  ,
    ( "581de0043849496f82deb0cfa0e6681adacd5e1f52dea714a748a4aae11aaa"
    , "stake_test1uqzrsj2fd7pdavx05rnxsxk6e40p75k75u22wj9y4ts342sjh0wy2"
    )
  ]

genByronAddress :: Gen (BootstrapAddress StandardCrypto)
genByronAddress = arbitrary

genShelleyAddress :: Gen (Addr StandardCrypto)
genShelleyAddress = Addr <$> arbitrary <*> arbitrary <*> arbitrary

genRewardAcnt :: Gen (RewardAcnt StandardCrypto)
genRewardAcnt = arbitrary

deserialiseBase16 :: FromCBOR a => Text -> a
deserialiseBase16 = unsafeDeserialize' . decodeLenient . encodeUtf8

decodeBase16 :: Decoding.DecCBOR a => Text -> a
decodeBase16 = deserialise' . decodeLenient . encodeUtf8
  where
    deserialise' = Decoding.unsafeDeserialize' Decoding.shelleyProtVer

getNetwork :: Addr c -> Network
getNetwork (AddrBootstrap _) = Mainnet
getNetwork (Addr net _ _) = net
