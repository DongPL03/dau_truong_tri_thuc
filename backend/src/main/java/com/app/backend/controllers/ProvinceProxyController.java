package com.app.backend.controllers;

import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.client.RestTemplate;

@RestController
@RequestMapping("${api.prefix}/provinces")
@RequiredArgsConstructor
public class ProvinceProxyController {
    private final RestTemplate restTemplate = new RestTemplate();

    @GetMapping("/p")
    public ResponseEntity<?> getProvinces() {
        String url = "https://provinces.open-api.vn/api/v2/p/";
        Object response = restTemplate.getForObject(url, Object.class);
        return ResponseEntity.ok(response);
    }

    @GetMapping("/w")
    public ResponseEntity<?> getWards() {
        String url = "https://provinces.open-api.vn/api/v2/w/";
        Object response = restTemplate.getForObject(url, Object.class);
        return ResponseEntity.ok(response);
    }
}
