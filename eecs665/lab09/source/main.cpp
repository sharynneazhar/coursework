class Class {
public:
    virtual ~Class();
    virtual void virtual_dispatch_method();
    virtual void virtual_dispatch_method_2();
    void static_dispatch_method();
};

extern "C" int make_static_call(Class *c) {
    c->static_dispatch_method();
    return 0;
}

extern "C" int make_virtual_call(Class *c) {
    c->virtual_dispatch_method();
    return 0;
}

extern "C" int make_virtual_call_2(Class *c) {
    c->virtual_dispatch_method_2();
    return 0;
}
