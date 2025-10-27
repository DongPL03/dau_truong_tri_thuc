package com.app.backend.repositories;

import com.app.backend.models.TraLoiLuyenTap;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface ITraLoiLuyenTapRepository extends JpaRepository<TraLoiLuyenTap, Long> {
    List<TraLoiLuyenTap> findByPhienLuyenTapId(Long phienLuyenTapId);
}
