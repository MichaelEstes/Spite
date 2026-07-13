package OS

extern
{
    #link windows "Rpcrt4";

    int32 UuidCreate(uuid: *byte);
}

extern
{
    #link linux "libuuid";

    void uuid_generate(uuid: *byte);
}

BCRYPT_USE_SYSTEM_PREFERRED_RNG := uint32(0x00000002);

[16]byte CreateUUID()
{
    create := #compile ::[16]byte()
    {
        if (targetOs == OS_Kind.Windows)
        {
            return ::[16]byte() {
                uuid := [16]byte;
                UuidCreate(fixed uuid);
                return uuid;
            }
        }
        else
        {
            return ::[16]byte() {
                uuid := [16]byte;
                uuid_generate(fixed uuid);
                return uuid;
            }
        }
    }

    return create();
}