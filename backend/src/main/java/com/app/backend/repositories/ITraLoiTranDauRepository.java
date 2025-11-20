package com.app.backend.repositories;

import com.app.backend.models.TraLoiTranDau;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface ITraLoiTranDauRepository extends JpaRepository<TraLoiTranDau, Long> {
    List<TraLoiTranDau> findAllByTranDau_Id(Long tranDauId);

    List<TraLoiTranDau> findByTranDau_IdAndNguoiDung_IdOrderByTraLoiLucAsc(Long tranDauId, Long nguoiDungId);
}
