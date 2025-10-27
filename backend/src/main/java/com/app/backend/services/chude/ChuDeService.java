package com.app.backend.services.chude;


import com.app.backend.dtos.ChuDeDTO;
import com.app.backend.models.ChuDe;

import com.app.backend.repositories.IChuDeRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
@RequiredArgsConstructor
public class ChuDeService implements IChuDeService {
    private final IChuDeRepository iChuDeRepository;

    @Override
    public ChuDe createChuDe(ChuDeDTO chuDe) {
        ChuDe newChuDe = ChuDe
                .builder()
                .ten(chuDe.getTen())
                .moTa(chuDe.getMoTa())
                .build();
        return iChuDeRepository.save(newChuDe);
    }

    @Override
    public ChuDe getChuDeById(long id) {
        return iChuDeRepository.findById(id).orElseThrow(() -> new RuntimeException("Chu De not found"));
    }

    @Override
    public ChuDe updateChuDe(long chuDeId, ChuDeDTO chuDe) {
        ChuDe existingChuDe = getChuDeById(chuDeId);
        existingChuDe.setTen(chuDe.getTen());
        existingChuDe.setMoTa(chuDe.getMoTa());
        return iChuDeRepository.save(existingChuDe);
    }

    @Override
    public ChuDe deleteChuDe(long id) throws Exception {
        return null;
    }

    @Override
    public List<ChuDe> getAllChuDe() {
        return iChuDeRepository.findAll();
    }
}
