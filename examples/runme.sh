exec ./dconv --dproto busted/test1/dproto/test1.dproto --asn1 busted/test1/asn/taste-types.asn \
        busted/test1/asn/taste-extended.asn busted/test1/asn/userdefs-base.asn busted/test1/asn/mybase.asn \
        --from Base_Pose lhs. --to kul_pose2 rhs. -o generated.c