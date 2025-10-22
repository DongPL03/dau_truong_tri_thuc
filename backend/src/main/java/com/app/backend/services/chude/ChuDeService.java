package com.app.backend.services.chude;


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
    public List<ChuDe> getAllChuDe() {
        return iChuDeRepository.findAll();
    }
}
