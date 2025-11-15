package com.app.backend.repositories;

import com.app.backend.models.LichSuTranDau;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface ILichSuTranDauRepository extends JpaRepository<LichSuTranDau, Long> {
    void deleteByTranDau_Id(Long tranDauId);
    List<LichSuTranDau> findAllByTranDau_Id(Long tranDauId);
}
