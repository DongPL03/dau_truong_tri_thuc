package com.app.backend.controllers;


import com.app.backend.models.ChuDe;
import com.app.backend.services.chude.ChuDeService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

@RestController
@RequiredArgsConstructor
@RequestMapping("${api.prefix}/chuDe")
public class ChuDeController {
    private final ChuDeService chuDeService;

    @GetMapping("")
    public ResponseEntity<List<ChuDe>> getAllChuDe() {
        List<ChuDe> chuDes = chuDeService.getAllChuDe();
        return new ResponseEntity<>(chuDes, HttpStatus.OK);
    }

}
