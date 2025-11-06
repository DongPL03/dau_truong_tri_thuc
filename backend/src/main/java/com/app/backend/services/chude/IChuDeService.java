package com.app.backend.services.chude;

import com.app.backend.dtos.ChuDeDTO;
import com.app.backend.models.ChuDe;

import java.util.List;

public interface IChuDeService {
    ChuDe createChuDe(ChuDeDTO chuDe);

    ChuDe getChuDeById(long id);

    ChuDe updateChuDe(long chuDeId, ChuDeDTO chuDe);

    ChuDe deleteChuDe(long id) throws Exception;

    List<ChuDe> getAllChuDe();
}
