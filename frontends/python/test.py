from bigchaindb_shared import call_json_rpc, BDBError


"""
The scope of these tests is not to check the functionality in detail,
but to check that basic API contract appears to be in place and does not
randomly break.
"""


pub = 'DD8qvyA6rXSTG4P1ojuFYvXUJ8UHnCy8srWE13xkZdvg'
sec = '4U6vaeue9wtBzkG1ybkShUyMDRgZKEjtQ2CAQQ5PFz67'

TX = {
    'id': '2981dfd996c6d5724bc205f78c7d808efba5bc6efc9ea0abf24c6003b913d0f0',
    'asset': {'data': {}},
    'inputs': [{
        'fulfillment': {
            'pubkeys': [pub],
            'structure': '%0'
        },
        'fulfills': None
    }],
    'metadata': {},
    'operation': 'CREATE',
    'outputs': [{
        'amount': '1',
        'condition': {
            'pubkeys': [pub],
            'structure': '%0'
        }
    }]
}

def test_generate_key_pair():
    assert {'public_key', 'secret_key'} == set(api.generateKeyPair({}))


def test_create_tx():
    res = api.createTx({'creator': pub, 'outputs': [["1", pub]]})
    assert res == TX


def test_sign_tx():
    signed = api.signTx({
        'tx': TX,
        'key': sec,
    })


def test_validate_tx():
    tx_bad = TX.copy()
    del tx_bad['asset']
    try:
        api.validateTx({'tx': tx_bad})
        assert False
    except BDBError as e:
        assert e.args[0] == 100


    tx_bad = TX.copy()
    tx_bad['id'] += 'a'
    try:
        api.validateTx({'tx': tx_bad})
        assert False
    except BDBError as e:
        assert e.args[0] == 100

    api.validateTx({'tx': TX})


def test_parse_condition_dsl():
    res = api.parseConditionDSL({
        'expr': 'DD8qvyA6rXSTG4P1ojuFYvXUJ8UHnCy8srWE13xkZdvg',
    })
    assert res == {
        'pubkeys': [pub],
        'structure': '%0'
        }
    res = api.parseConditionDSL({
        'expr': ('(2 of DD8qvyA6rXSTG4P1ojuFYvXUJ8UHnCy8srWE13xkZdvg)'),
    })
    assert res == {
        'pubkeys': [pub],
        'structure': '(2 of %0)'
    }


def test_parse_condition_dsl_fail():
    try:
        api.parseConditionDSL({
            'expr': 'fds',
        })
        assert False
    except BDBError as e:
        assert e.args[0] == 101


def test_verify_fulfillment():
    ffill = api.signCondition({
        'keys': [sec],
        'condition': TX['outputs'][0]['condition'],
        'msg': 'hello',
    })
    res = api.verifyFulfillment({
        'fulfillment': ffill,
        'msg': 'hello',
        'condition': TX['outputs'][0]['condition'],
    })
    assert res == {'valid': True}
        
    res = api.verifyFulfillment({
        'fulfillment': ffill,
        'msg': 'wat',
        'condition': TX['outputs'][0]['condition'],
    })
    assert res == {'valid': False}


class API(object):
    def __getattr__(self, name):
        return lambda val: API.call(name, val)
    
    @staticmethod
    def call(name, val):
        return call_json_rpc(name, val)

api = API()
