package com.app.backend.repositories;

import com.app.backend.models.TheGhiNho;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface ITheGhiNhoRepository extends JpaRepository<TheGhiNho, Long> {
    List<TheGhiNho> findByPhienId(Long phienId);
    boolean existsByPhienIdAndCauHoiId(Long phienId, Long cauHoiId);
}
